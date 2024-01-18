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

object ActivityManager {
  private case object TimerKey

  trait MState

  case object MIdle extends MState

  case object MDispatching extends MState

  case object MWaitingForPlan extends MState

  sealed trait MData

  case object MNothing extends MData

  case class MPlanner(planner: Planner) extends MData

  case class MPendingGoals(state: PPlan, pendingGoals: List[String])
      extends MData

  case class FullPlan(val plan: PPlan) extends MData {
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

  def apply(): Behavior[ManagerEvent] = Behaviors.setup { context =>
    val planner = context.spawn(PlanningActor(), name = "planner")
    val dispatcher = context.spawn(DispatchActor(), name = "dispatcher")
    Behaviors.withTimers(timers =>
      new ActivityManager(timers, planner, dispatcher).idle(MNothing)
    )
  }

}

import fr.laas.fape.acting.ActivityManager._

class ActivityManager(
    timers: TimerScheduler[ManagerEvent],
    planner: ActorRef[PlannerEvent],
    dispatcher: ActorRef[DispatchEvent]
) {

  timers.startTimerWithFixedDelay(TimerKey, Tick, 0.seconds, Clock.interval.milli)

  private var currentReqID = -1
  private val goals = new ArrayBuffer[PartialPlanModification]()
  private val executed = mutable.Map[TPRef, Int]()
  private val notifiedActive = mutable.Set[TPRef]()

  private def getReqID: Int = {
    currentReqID += 1
    currentReqID
  }

  private def t = Clock.time

  private def idle(data: MData): Behavior[ManagerEvent] = Behaviors.setup { context =>
    Behaviors.receiveMessage[ManagerEvent] { message =>
      (message, data) match {
        case (AddGoal(goal), MNothing) =>
          integrateNewGoal(goal, context)
          waitingForPlan(MNothing)
        case (Tick, _) => 
          Clock.advanceTime
          context.log.debug(s"[$t] Idle")
          idle(MNothing)
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
          case (NoPlanExists(reqID), MNothing) =>
            context.log.info(s"No plan exists for request $reqID")
            idle(MNothing)
          case (PlanFound(sol, reqID), MNothing) =>
            if (reqID < currentReqID)
              waitingForPlan(MNothing)
            else {
              if (!executed.contains(sol.pb.start))
                executed += ((sol.pb.start, 0))
              // Set all actions to pending
              sol.getAllActions.asScala.foreach(
                _.setStatus(ActionStatus.PENDING)
              )
              dispatching(FullPlan(sol))
            }
          case (Tick, MNothing) => 
            Clock.advanceTime
            context.log.debug(s"[$t] Waiting for plan")
            waitingForPlan(MNothing)
            
        }
      }
  }

  private def dispatching(data: MData): Behavior[ManagerEvent] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[ManagerEvent] { message =>
        (message, data) match {
          case (
                Tick,
                x: FullPlan
              ) => // there should be no pending goals while dispatching
            
            Clock.advanceTime

            context.log.debug(s"[$t] Current State: ")
            val actions = Printer.actionsInPlan(x.plan).split("\n")
            actions.foreach(context.log.debug(_))
            // x.plan.getAllActions.asScala.foreach(a => context.log.debug(s"${a.name} ${a.start} ${a.end}"))

            // println( x.plan.csp.stn.timepoints.asScala.toString)
            val dispatchNet = DispatchableNetwork.getDispatchableNetwork(
              x.plan.csp.stn,
              x.plan.csp.stn.timepoints.asScala.toSet.asJava
            )
            for (tp <- executed.keys) {
              if (!dispatchNet.isExecuted(tp)) {
                try {
                  dispatchNet.setExecuted(tp, executed(tp))
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
            if (dispatchNet.isExecuted(x.plan.pb.end)) {
              context.log.info(s"[$t] Plan executed successfully, going idle")
              idle(MNothing)
            } else {
              context.log.debug(s"[$t] Executed: ${executed.mkString(", ")}")
              val executables = dispatchNet.getExecutables(t).asScala
              context.log.debug(
                s"[$t] Executables: ${executables.mkString(", ")}"
              )
              for (tp <- executables) {
                if (x.actionStarts.contains(tp)) {
                  context.log.info(
                    s"[$t] Starting action: ${Printer.action(x.plan, x.actionStarts(tp))}"
                  )
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
                  dispatchNet.setExecuted(tp, t)
                  executed += ((tp, t))
                }
              }
              dispatching(x)
            }

          case (DispatchSuccess(a), x: FullPlan) =>
            val t = Clock.time()
            context.log.info(s"[$t] Action ${Printer.action(x.plan, a)} executed successfully")
            val earliestStart = x.plan.getEarliestStartTime(a.end)
            if (t < earliestStart) {
              executed += ((a.end, earliestStart))
            } else {
              executed += ((a.end, t))
            }

            dispatching(x)
        }
      }
  }
  //  whenUnhandled {
  //    case Event(TimepointExecuted(tp, time), _) =>
  //      executed += ((tp, time))
  //      stay()
  //  }

  private def getInitialPartialPlan = {
    val pplan =
      new PPlan(Utils.getProblem, Controllability.PSEUDO_CONTROLLABILITY)
    for (g <- goals)
      pplan.apply(g, false)
    pplan
  }

  private def integrateNewGoal(
      goal: PartialPlanModification,
      context: ActorContext[ManagerEvent]
  ): Unit = {
    goals += goal
    val pplan = getInitialPartialPlan
    planner ! GetPlan(
      pplan,
      FiniteDuration(10, TimeUnit.SECONDS),
      getReqID,
      context.self
    )
  }

}
