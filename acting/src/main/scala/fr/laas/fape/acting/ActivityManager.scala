package fr.laas.fape.acting

import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import fr.laas.fape.acting.messages._
import fr.laas.fape.anml.model.concrete.{Action, TPRef}
import fr.laas.fape.constraints.stnu.Controllability
import fr.laas.fape.constraints.stnu.dispatching.DispatchableNetwork
import fr.laas.fape.planning.core.planning.planner.Planner
import fr.laas.fape.planning.core.planning.states.modification.PartialPlanModification
import fr.laas.fape.planning.core.planning.states.{
  Printer,
  PartialPlan => PPlan
}

import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import fr.laas.fape.anml.model.concrete.ActionStatus
import fr.laas.fape.planning.core.planning.states.modification.ActionInsertion

object ActivityManager {
  private case object TimerKey

  trait MState

  case object MIdle extends MState

  case object MDispatching extends MState

  case object MWaitingForPlan extends MState

  sealed trait MData

  case object MNothing extends MData

  // case class MPlanner(planner: Planner) extends MData

  // case class MPendingGoals(state: PPlan, pendingGoals: List[String])
  //     extends MData

  case class FullPlan(val plan: PPlan, val network: DispatchableNetwork)
      extends MData {
    lazy val actionStarts: Map[TPRef, Action] =
      plan.getAllActions.asScala.map(a => (a.start, a)).toMap
    lazy val actionEnds: Map[TPRef, Action] =
      plan.getAllActions.asScala.map(a => (a.end, a)).toMap

    // maps every dispatchable timepoint inside an action to its containing action
    lazy val actionsInternalTimepoints: Map[TPRef, Action] =
      plan.getAllActions.asScala
        .flatMap(action =>
          action.usedVariables.collect {
            case tp: TPRef if tp.genre.isDispatchable => (tp, action)
          }
        )
        .filter(tpActPair =>
          tpActPair._1 != tpActPair._2.start && tpActPair._1 != tpActPair._2.end
        )
        .toMap
  }

  case class SetGoal(goal: String)

  def apply(clock: ActorRef[ClockEvent]): Behavior[ManagerEvent] =
    Behaviors.setup { context =>
      clock ! RegisterTickListener(context.self)
      val planner = context.spawn(PlanningActor(), name = "planner")
      val dispatcher = context.spawn(DispatchActor(clock), name = "dispatcher")
      // FIXME: this is a hack to make the planner and dispatcher actors available in the ActivityManager. Not sure if this is the best way to do it.
      new ActivityManager(clock, planner, dispatcher).idle(MNothing)
    }

}

import fr.laas.fape.acting.ActivityManager._

class ActivityManager(
    clock: ActorRef[ClockEvent],
    planner: ActorRef[PlannerEvent],
    dispatcher: ActorRef[DispatchEvent]
) {

  private var currentReqID = -1
  private val goals = new ArrayBuffer[PartialPlanModification]()
  private val executed = mutable.Map[TPRef, Int]()
  private val notifiedActive = mutable.Set[TPRef]()
  private val executedActions = new ArrayBuffer[Action]()
  private var goIdle = true

  private def getReqID: Int = {
    currentReqID += 1
    currentReqID
  }

  private def idle(data: MData): Behavior[ManagerEvent] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[ManagerEvent] { message =>
        (message, data) match {
          case (AddGoal(goal), MNothing) =>
            clock ! StopClock
            val newPlan = integrateNewGoal(goal, context)
            waitingForPlan(newPlan)
          case (AddGoal(goal), x: FullPlan) =>
            clock ! StopClock
            val newPlan = integrateNewGoal(goal, context, Some(x))
            waitingForPlan(newPlan)
          case (Tick(t), x) =>
            context.log.debug(s"[$t] Idle")
            if (goIdle) {
              idle(x)
            } else {
              context.log.info("Shutting down from idle")
              Behaviors.stopped
            }
          case (ShutdownAfterFinished, _) =>
            goIdle = false
            Behaviors.same
        }
      }
  }

  private def waitingForPlan(data: MData): Behavior[ManagerEvent] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[ManagerEvent] { message =>
        (message, data) match {
          case (AddGoal(goal), MNothing) =>
            clock ! StopClock
            val newPlan = integrateNewGoal(goal, context)
            waitingForPlan(newPlan)
          case (AddGoal(goal), x: FullPlan) =>
            clock ! StopClock
            val newPlan = integrateNewGoal(goal, context, Some(x))
            waitingForPlan(newPlan)
          case (PlanFound(sol, reqID), x) =>
            if (reqID < currentReqID)
              waitingForPlan(x)
            else {
              if (!executed.contains(sol.pb.start))
                executed += ((sol.pb.start, 0))
              context.log.info(s"Plan: ")
              val actions = Printer.actionsInPlan(sol).split("\n")
              actions.foreach(context.log.info(_))
                      
              val timelines = Printer.timelines(sol).split("\n")
              timelines.foreach(context.log.info(_))
              val dispatchNet = DispatchableNetwork.getDispatchableNetwork(
                sol.csp.stn,
                sol.csp.stn.timepoints.asScala.toSet.asJava
              )
              clock ! ResumeClock
              dispatching(FullPlan(sol, dispatchNet))
            }
          case (Tick(t), x) =>
            context.log.debug(s"[$t] Waiting for plan")
            waitingForPlan(x)
          case (DispatchSuccess(a, t), x: FullPlan) =>
            context.log.info(
              s"[$t] Action ${Printer.action(x.plan, a)} executed successfully while replanning"
            )
            if (!executed.contains(a.end)) {
              executed += ((a.end, t))
            }
            waitingForPlan(x)
          case (ShutdownAfterFinished, _) =>
            goIdle = false
            Behaviors.same
          // Errors
          case (NoPlanExists(reqID), x: FullPlan) =>
            context.log.error(s"No plan exists for request $reqID, trying replanning")
            planner ! TryReplan(
              x.plan,
              FiniteDuration(Utils.planningTimeout, TimeUnit.SECONDS),
              0,
              getReqID,
              context.self
            )
            Behaviors.same
          case (ReplanFailed(reqID), _) =>
            context.log.error(s"Replanning failed for request $reqID, there is no way to procede")
            Behaviors.stopped
          case (RepairFailed(reqID), x: FullPlan) =>
            context.log.error(s"Repair failed for request $reqID, trying replanning")
            planner ! TryReplan(
              x.plan,
              FiniteDuration(Utils.planningTimeout, TimeUnit.SECONDS),
              0,
              getReqID,
              context.self
            )
            Behaviors.same
          case (PlanningTimedOut(reqID), _) =>
            context.log.error(s"Planning timed out for request $reqID")
            Behaviors.stopped
        }
      }
    }

  private def dispatching(data: MData): Behavior[ManagerEvent] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[ManagerEvent] { message =>
        (message, data) match {
          case (
                Tick(t),
                x: FullPlan
              ) =>
            context.log.debug(s"[$t] Current State: ")
            val actions = Printer.actionsInPlan(x.plan).split("\n")
            actions.foreach(context.log.debug(_))

            if (executed.contains(x.plan.pb.end)) {
              printPlanExecuted(x, context)
              if (goIdle) {
                idle(x)
              } else {
                context.log.info("Shutting down from dispatching")
                Behaviors.stopped
              }
            } else { 
              updateNetwork(t, x, context)
              context.log.trace(s"[$t] Executed: ${executed.mkString(", ")}")
              dispatchExecutableActions(t, x, context)
              dispatching(x)
            }

          case (DispatchSuccess(a, t), x: FullPlan) =>
            context.log.info(
              s"[$t] Action ${Printer.action(x.plan, a)} executed successfully"
            )
            if (!executed.contains(a.end)) {
              executed += ((a.end, t))
            }

            if (executed.contains(x.plan.pb.end)) {
              printPlanExecuted(x, context)
              if (goIdle) {
                idle(x)
              } else {
                context.log.info("Shutting down from dispatching")
                Behaviors.stopped
              }
            } else {

              updateNetwork(t, x, context)
              context.log.trace(s"[$t] Executed: ${executed.mkString(", ")}")
              dispatchExecutableActions(t, x, context)
              dispatching(x)
            }

          case (AddGoal(goal), x: FullPlan) =>
            clock ! StopClock
            val newPlan = integrateNewGoal(goal, context, Some(x))
            waitingForPlan(newPlan)
          case (ShutdownAfterFinished, _) =>
            goIdle = false
            Behaviors.same
        }
      }
    }

  private def updateNetwork(
      t: Int,
      x: FullPlan,
      context: ActorContext[ManagerEvent]
  ) = {
    for (tp <- executed.keys) {
      if (!x.network.isExecuted(tp)) {
        try {
          x.network.setExecuted(tp, executed(tp))
        } catch {
          case e: Exception =>
            context.log.error(
              s"[$t] Error while setting executed timepoint $tp to ${executed(tp)}"
            )
            context.log.debug(s"[$t] Executed: ${executed.mkString(", ")}")
            throw e
        }
      }
    }

    // Set empty structural actions as executing/executed
    for (a <- x.plan.getAllActions.asScala) {
      if (a.isStructural && a.tasks.isEmpty ) {
        if (a.status == ActionStatus.PENDING && x.plan.getEarliestStartTime(a.start) <= t)
          Utils.setExecuting(a)
        if (a.status == ActionStatus.EXECUTING && x.plan.getEarliestStartTime(a.end) <= t )
          Utils.setExecuted(a, x.plan)
      }
    }
  }

  private def dispatchExecutableActions(
      t: Int,
      x: FullPlan,
      context: ActorContext[ManagerEvent]
  ) = {
    val executables =
      x.network.getExecutables(t).asScala.filter(!executed.contains(_))
    context.log.trace(
      s"[$t] Executables: ${executables.mkString(", ")}"
    )
    for (tp <- executables) {
      if (x.actionStarts.contains(tp)) {
        context.log.info(
          s"[$t] Starting action: ${Printer.action(x.plan, x.actionStarts(tp))}"
        )
        executedActions += x.actionStarts(tp)
        executed += ((tp, t))
        dispatcher ! ExecutionRequest(
          x.actionStarts(tp),
          x.plan,
          context.self
        )
      } else if (x.actionsInternalTimepoints.contains(tp)) {
        if (!notifiedActive.contains(tp)) {
          context.log.info(s"[$t] Notifying of active timepoint")
          notifiedActive.add(tp)
          dispatcher ! ActiveTimepointNotification(
            x.actionsInternalTimepoints(tp).id,
            TimepointActive(tp)
          )
        }
      } else if (!x.actionEnds.contains(tp)) {
        context.log.debug(s"[$t] Timpoint $tp marked as executed")
        executed += ((tp, t))
      } else {
        context.log.debug(
          s"[$t] Timepoint $tp is not dispatchable by the manager"
        )
      }
    }
  }

  private def printPlanExecuted(x: FullPlan, context: ActorContext[ManagerEvent]) = {
    context.log.warn("Plan executed successfully, going idle")
    context.log.info("Final timeline: ")
    val actions = Printer.actionsInPlan(x.plan).split("\n")
    actions.foreach(context.log.info(_))
    val timelines = Printer.timelines(x.plan).split("\n")
    timelines.foreach(context.log.debug(_))
  }

  private def getInitialPartialPlan = {
    val pplan =
      new PPlan(Utils.getProblem, Controllability.PSEUDO_CONTROLLABILITY)
    for (g <- goals)
      pplan.apply(g, false)
    // for (a <- executedActions)
    //   pplan.apply(new ActionInsertion(a), false)
    pplan
  }

  private def integrateNewGoal(
      goal: PartialPlanModification,
      context: ActorContext[ManagerEvent],
      currentPlan: Option[FullPlan] = None
  ): FullPlan = {
    var currentTime = -1
    val pplan = currentPlan match {
      case Some(FullPlan(plan, network)) =>
        context.log.info("Integrating new goal into existing plan")
        currentTime = network.getCurrentTime
        goals += goal
        executed -= plan.pb.end
        val newPlan = plan.cc(false)
        try {
          val consistent = newPlan.apply(goal, false)
          if (!consistent) {
            // FIXME: maybe this could be fixable, but unsure
            context.log.info(
              "New goal is not consistent with current plan, planning will fail"
            )
          }
        } catch {
          case e: Exception =>
            context.log.error(
              s"Error while applying goal $goal with exception $e"
            )
            println(newPlan.csp.stn.toStringRepresentation)
            throw e
        }

        newPlan
      case None =>
        context.log.info("Recreating plan from scratch")
        goals += goal
        getInitialPartialPlan
    }
    planner ! GetPlan(
      pplan,
      FiniteDuration(Utils.planningTimeout, TimeUnit.SECONDS),
      currentTime,
      getReqID,
      context.self
    )
    return FullPlan(pplan.cc(false), DispatchableNetwork.getDispatchableNetwork(pplan.csp.stn, pplan.csp.stn.timepoints.asScala.toSet.asJava))
  }

}
