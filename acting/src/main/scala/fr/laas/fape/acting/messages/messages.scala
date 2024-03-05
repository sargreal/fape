package fr.laas.fape.acting.messages

import akka.actor.typed.ActorRef
import fr.laas.fape.anml.model.concrete.{Action, ActRef, TPRef}
import fr.laas.fape.planning.core.execution.model.AtomicAction
import fr.laas.fape.planning.core.planning.states.PartialPlan
import fr.laas.fape.planning.core.planning.states.modification.PartialPlanModification

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration


sealed trait Event

sealed trait ClockEvent extends Event
case object InternalTick extends ClockEvent
// case class GetTime(actorRef: ActorRef[Time]) extends ClockEvent
case class RegisterTickListener(actorRef: ActorRef[Tick]) extends ClockEvent
case class UnregisterTickListener(actorRef: ActorRef[Tick]) extends ClockEvent
case class ReplyAt(val timepoint: Int, val reference:Int, val actorRef: ActorRef[TimedReply]) extends ClockEvent
case object StopClock extends ClockEvent
case object ResumeClock extends ClockEvent

sealed trait ClockReply extends Event
case class Tick(timepoint: Int) extends ClockReply with ManagerEvent with DispatchEvent
// case class Time(timepoint: Int) extends ClockReply with ManagerEvent
case class TimedReply(timepoint: Int, reference: Int) extends ClockReply with DispatchEvent

sealed trait ManagerEvent extends Event

final case class AddGoal(goal: PartialPlanModification) extends ManagerEvent
final case class TimepointExecuted(tp: TPRef, time: Int) extends ManagerEvent
final case class TimepointActive(tp: TPRef) extends ManagerEvent
object ShutdownAfterFinished extends ManagerEvent

sealed trait PlannerEvent extends Event

object GetPlan extends PlannerEvent
case class GetPlan(state: PartialPlan, forHowLong: FiniteDuration, currentTime: Int, reqID: Int, actorRef: ActorRef[PlannerReply], previousUnoptimizedPlan: Option[PartialPlan] = None) extends PlannerEvent
case class TryRepair(state: PartialPlan, forHowLong: FiniteDuration, reqID: Int) extends PlannerEvent
case class TryReplan(state: PartialPlan, forHowLong: FiniteDuration, reqID: Int) extends PlannerEvent
case object RepairFailed extends PlannerEvent
case object ReplanFailed extends PlannerEvent

sealed trait PlannerReply extends ManagerEvent
case class PlanFound(state: PartialPlan, reqID: Int) extends PlannerReply
case class NoPlanExists(reqID: Int) extends PlannerReply
case class PlanningTimedOut(reqID: Int) extends PlannerReply

sealed trait DispatchEvent extends Event

case class ActiveTimepointNotification(action: ActRef, timepointActive: TimepointActive) extends DispatchEvent with ClockReply
case class ExecutionRequest(action: Action, plan: PartialPlan, actorRef: ActorRef[DispatchReply]) extends DispatchEvent with ClockReply {

  def name = action.name
  def parameters = action.args.asScala.map(a => plan.valuesOf(a).get(0).instance).toList
}

sealed trait DispatchReply extends ManagerEvent

case class DispatchSuccess(a: Action, timepoint: Int) extends DispatchReply
case class DispatchFailed(req: ExecutionRequest) extends DispatchReply
// case class DispatchExecute(a: AtomicAction)

object AAction {
  def apply(id: ActRef, name: String, params: Seq[String], start: Int, minDur: Int, maxDur: Int) = new AtomicAction(id, name, params.asJava, start, minDur, maxDur)
  def unapply(a: AtomicAction) : Option[(ActRef, String, Seq[String], Int, Int, Int)] = Some(a.id, a.name, a.params.asScala.toList, a.mStartTime, a.minDuration, a.maxDuration)
}




sealed trait ObserverMessage
object GetProblemFromScene extends ObserverMessage
case class ProblemFromScene(anml: String) extends ObserverMessage
object ErrorOnProblemGeneration extends ObserverMessage

