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

  case class FullPlan(val plan: PPlan, val network: DispatchableNetwork) extends MData {
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

  def apply(clock: ActorRef[ClockEvent]): Behavior[ManagerEvent] = Behaviors.setup { context =>
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

  private def getReqID: Int = {
    currentReqID += 1
    currentReqID
  }

  private def idle(data: MData): Behavior[ManagerEvent] = Behaviors.setup { context =>
    Behaviors.receiveMessage[ManagerEvent] { message =>
      (message, data) match {
        case (AddGoal(goal), MNothing) =>
          integrateNewGoal(goal, context)
          waitingForPlan(MNothing)
        case (AddGoal(goal), x: FullPlan) =>
          integrateNewGoal(goal, context, x.plan)
          waitingForPlan(x)
        case (Tick(t), x) => 
          context.log.debug(s"[$t] Idle")
          idle(x)
      }
    }
  }

  private def waitingForPlan(data: MData): Behavior[ManagerEvent] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[ManagerEvent] { message =>
        (message, data) match {
          case (AddGoal(goal), MNothing) =>
            integrateNewGoal(goal, context)
            waitingForPlan(MNothing)
          case (AddGoal(goal), x: FullPlan) =>
            integrateNewGoal(goal, context, x.plan)
            waitingForPlan(x)
          case (NoPlanExists(reqID), x) =>
            context.log.info(s"No plan exists for request $reqID")
            if (reqID < currentReqID)
              waitingForPlan(x)
            else
              idle(x)
          case (PlanFound(sol, reqID), x) =>
            if (reqID < currentReqID)
              waitingForPlan(x)
            else {
              if (!executed.contains(sol.pb.start))
                executed += ((sol.pb.start, 0))
              context.log.info(s"Plan: ")
              val actions = Printer.actionsInPlan(sol).split("\n")
              actions.foreach(context.log.info(_))
              val dispatchNet = DispatchableNetwork.getDispatchableNetwork(
                sol.csp.stn,
                sol.csp.stn.timepoints.asScala.toSet.asJava
              )
              dispatching(FullPlan(sol, dispatchNet))
            }
          case (Tick(t), x) => 
            context.log.debug(s"[$t] Waiting for plan")
            waitingForPlan(x)
          case (DispatchSuccess(a, t), x: FullPlan) =>
            context.log.info(s"[$t] Action ${Printer.action(x.plan, a)} executed successfully while replanning")
            if (!executed.contains(a.end)) {
              executed += ((a.end, t))
            }
            waitingForPlan(x)
            
        }
      }
  }

  private def dispatching(data: MData): Behavior[ManagerEvent] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[ManagerEvent] { message =>
        (message, data) match {
          case (
                Tick(t),
                x: FullPlan
              ) => // there should be no pending goals while dispatching

            context.log.debug(s"[$t] Current State: ")
            val actions = Printer.actionsInPlan(x.plan).split("\n")
            actions.foreach(context.log.debug(_))
            // x.plan.getAllActions.asScala.foreach(a => context.log.debug(s"${a.name} ${a.start} ${a.end}"))

            
            if (executed.contains(x.plan.pb.end)) {
              context.log.info(s"[$t] Plan executed successfully, going idle")
              context.log.info(s"[$t] Final timeline: ")
              val actions = Printer.actionsInPlan(x.plan).split("\n")
              actions.foreach(context.log.info(_))
              idle(x)
            } else {
              // println( x.plan.csp.stn.timepoints.asScala.toString)
              
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
            
              context.log.trace(s"[$t] Executed: ${executed.mkString(", ")}")
              val executables = x.network.getExecutables(t).asScala
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
                } else if (!x.actionEnds.contains(tp) && !executed.contains(tp)) {
                  // dispatchNet.setExecuted(tp, t)
                  context.log.debug(s"[$t] Timpoint $tp marked as executed")
                  executed += ((tp, t))
                } else {
                  context.log.debug(s"[$t] Timepoint $tp is not dispatchable by the manager")
                }
              }
              dispatching(x)
            }

          case (DispatchSuccess(a, t), x: FullPlan) =>
            context.log.info(s"[$t] Action ${Printer.action(x.plan, a)} executed successfully")
            if (!executed.contains(a.end)) {
              executed += ((a.end, t))
            }
            dispatching(x)
          
          case (AddGoal(goal), x: FullPlan) =>
            integrateNewGoal(goal, context, x.plan)
            waitingForPlan(x)
        }
      }
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
      currentPlan: PPlan = null
  ): Unit = {
    val pplan = if (currentPlan != null) {
      context.log.info("Integrating new goal into existing plan")
      goals += goal
      executed -= currentPlan.pb.end
      val newPlan = currentPlan.cc(false)
      try {
        val consistent = newPlan.apply(goal, false)
        if (!consistent) {
          // FIXME: maybe this could be fixable, but unsure
          context.log.info("New goal is not consistent with current plan, planning will fail")
        }
      } catch {
        case e: Exception =>
          context.log.error(s"Error while applying goal $goal with exception $e")
          println(newPlan.csp.stn.toStringRepresentation)
          throw e
      }
      
      newPlan
    } else {
      context.log.info("Recreating plan from scratch")
      goals += goal
      getInitialPartialPlan
    }
    // goals += goal
    // val pplan =  getInitialPartialPlan
    planner ! GetPlan(
      pplan,
      FiniteDuration(10, TimeUnit.SECONDS),
      getReqID,
      context.self
    )
  }

}
