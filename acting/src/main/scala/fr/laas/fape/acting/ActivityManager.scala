package fr.laas.fape.acting

import java.util.concurrent.TimeUnit
import akka.actor.SupervisorStrategy._
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, LoggerOps, TimerScheduler}
import akka.event.Logging
import fr.laas.fape.acting.PlanningActor.PlanFound
import fr.laas.fape.acting.actors.patterns.MessageLogger
import fr.laas.fape.acting.messages.{AddGoal, Event, ExecutionRequest, GetPlan, PlanFound, PlannerMessage, Tick, TimepointActive, TimepointExecuted}
import fr.laas.fape.anml.model.AnmlProblem
import fr.laas.fape.anml.model.concrete.{Action, TPRef}
import fr.laas.fape.constraints.stnu.Controllability
import fr.laas.fape.constraints.stnu.dispatching.DispatchableNetwork
import fr.laas.fape.planning.core.planning.planner.Planner
import fr.laas.fape.planning.core.planning.states.modification.PartialPlanModification
import fr.laas.fape.planning.core.planning.states.{Printer, PartialPlan => PPlan}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.collection.mutable

case class WholeProblem(anmlFile: String)

object ActivityManager {
  private case object TimerKey

  trait MState

  case object MIdle extends MState

  case object MDispatching extends MState

  case object MWaitingForPlan extends MState


  sealed trait MData

  case object MNothing extends MData

  case class MPlanner(planner: Planner) extends MData

  case class MPendingGoals(state: PPlan, pendingGoals: List[String]) extends MData

  case class FullPlan(val plan: PPlan) extends MData {
    lazy val actionStarts: Map[TPRef, Action] = plan.getAllActions.asScala.map(a => (a.start, a)).toMap
    lazy val actionEnds: Map[TPRef, Action] = plan.getAllActions.asScala.map(a => (a.end, a)).toMap

    // maps every dispatchable timepoint inside an action to its containing action
    lazy val actionsInternalTimepoints: Map[TPRef, Action] = plan.getAllActions.asScala
      .flatMap(action => action.usedVariables.collect { case tp: TPRef if tp.genre.isDispatchable => (tp, action) })
      .filter(tpActPair => tpActPair._1 != tpActPair._2.start && tpActPair._1 != tpActPair._2.end)
      .toMap
  }

  case class SetGoal(goal: String)

  def apply(): Behavior[Event] = Behaviors.setup { context =>
    val planner = context.spawn(PlanningActor(), name = "planner")
    Behaviors.withTimers(timers => new ActivityManager(timers, planner).idle(MNothing))
  }

}

import fr.laas.fape.acting.ActivityManager._

class ActivityManager(timers: TimerScheduler[Event], planner: ActorRef[PlannerMessage]) {

  timers.startTimerAtFixedRate(TimerKey, Tick, 0.seconds, 0.01.seconds)

  private var currentReqID = -1
  private val goals = new ArrayBuffer[PartialPlanModification]()
  private val executed = mutable.Map[TPRef, Int]()
  private val notifiedActive = mutable.Set[TPRef]()

  private def getReqID: Int = {
    currentReqID += 1
    currentReqID
  }


  private def idle(data: MData): Behavior[Event] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[Event] {
        message =>
          (message, data) match {
            case (AddGoal(goal), MNothing) =>
              integrateNewGoal(goal, context)
              waitingForPlan(MNothing)
            case (Tick, _) => idle(MNothing)
          }
      }
  }

  private def waitingForPlan(data: MData): Behavior[Event] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[Event] {
        message =>
          (message, data) match {
            case (AddGoal(goal), MNothing) =>
              integrateNewGoal(goal, context)
              waitingForPlan(MNothing)
            case (PlanFound(sol, reqID), MNothing) =>
              if (reqID < currentReqID)
                waitingForPlan(MNothing)
              else {
                if (!executed.contains(sol.pb.start))
                  executed += ((sol.pb.start, 0))
                dispatching(FullPlan(sol))
              }

          }
      }
  }

  private def dispatching(data: MData): Behavior[Event] = Behaviors.setup {
    context =>
      Behaviors.receiveMessage[Event] {
        message =>
          (message, data) match {
            case (Tick, x: FullPlan) => // there should be no pending goals while dispatching
              val dispatcher = DispatchableNetwork.getDispatchableNetwork(x.plan.csp.stn, x.plan.csp.stn.timepoints.asScala.toSet.asJava)
              for (tp <- executed.keys) {
                if (!dispatcher.isExecuted(tp))
                  dispatcher.setExecuted(tp, executed(tp))
              }
              val t = Clock.time()
              val executables = dispatcher.getExecutables(t).asScala
              for (tp <- executables) {
                if (x.actionStarts.contains(tp)) {
                  context.log.info(s"[$t] Starting action: ${Printer.action(x.plan, x.actionStarts(tp))}")
                  executed += ((tp, Clock.time()))
                  actionDispatcher ! new ExecutionRequest(x.actionStarts(tp), x.plan, self)
                } else if (x.actionsInternalTimepoints.contains(tp)) {
                  if (!notifiedActive.contains(tp)) {
                    context.log.info(s"[$t] Notifying of active timepoint")
                    notifiedActive.add(tp)
                    actionDispatcher ! (x.actionsInternalTimepoints(tp).name, TimepointActive(tp))
                  }
                } else if (!x.actionEnds.contains(tp)) {
                  dispatcher.setExecuted(tp, t)
                  executed += ((tp, t))
                }
              }
              dispatching(x)
          }
      }
  }

  //  override val supervisorStrategy =
  //    OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1.minute) {
  //      case _: ActorKilledException      => Restart
  //      case e                            => Restart
  //    }

  //  val timeManager = context.actorOf(Props[TemporalConstraintsManager], name = "time-manager")

  val actionDispatcher = context.actorSelection("../actor")
  //  val observer = context.actorSelection("../observer")

  //  whenUnhandled {
  //    case Event(TimepointExecuted(tp, time), _) =>
  //      executed += ((tp, time))
  //      stay()
  //  }

  private def getInitialPartialPlan = {
    val pplan = new PPlan(Utils.getProblem, Controllability.PSEUDO_CONTROLLABILITY)
    for (g <- goals)
      pplan.apply(g, false)
    pplan
  }

  private def integrateNewGoal(goal: PartialPlanModification, context: ActorContext[Event]): Unit = {
    goals += goal
    val pplan = getInitialPartialPlan
    planner ! GetPlan(pplan, FiniteDuration(10, TimeUnit.SECONDS), getReqID, context.self)
  }

}