package fr.laas.fape.acting.messages

import akka.actor.typed.ActorRef
import fr.laas.fape.anml.model.concrete.{ActRef, TPRef}
import fr.laas.fape.planning.core.execution.model.AtomicAction
import fr.laas.fape.planning.core.planning.states.PartialPlan
import fr.laas.fape.planning.core.planning.states.modification.PartialPlanModification

import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration


sealed trait Event

case object ENothing extends Event
final case class AddGoal(goal: PartialPlanModification) extends Event
final case class TimepointExecuted(tp: TPRef, time: Int) extends Event
final case class TimepointActive(tp: TPRef) extends Event
case object Tick extends Event

sealed trait PlannerMessage extends Event

object GetPlan extends PlannerMessage
case class GetPlan(state: PartialPlan, forHowLong: FiniteDuration, reqID: Int, actorRef: ActorRef[PlannerReply]) extends PlannerMessage
case class TryRepair(state: PartialPlan, forHowLong: FiniteDuration, numPlanReq: Int) extends PlannerMessage
case class TryReplan(state: PartialPlan, forHowLong: FiniteDuration, numPlanReq: Int) extends PlannerMessage
case object RepairFailed extends PlannerMessage
case object ReplanFailed extends PlannerMessage
sealed trait PlannerReply extends PlannerMessage
case class PlanFound(state: PartialPlan, numPlanReq: Int) extends PlannerReply
case class NoPlanExists(reqID: Int) extends PlannerReply
case class PlanningTimedOut(reqID: Int) extends PlannerReply

case class Success(a: AtomicAction)
case class Failed(req: ExecutionRequest)
case class Execute(a: AtomicAction)

object AAction {
  def apply(id: ActRef, name: String, params: Seq[String], start: Int, minDur: Int, maxDur: Int) = new AtomicAction(id, name, params.asJava, start, minDur, maxDur)
  def unapply(a: AtomicAction) : Option[(ActRef, String, Seq[String], Int, Int, Int)] = Some(a.id, a.name, a.params.asScala.toList, a.mStartTime, a.minDuration, a.maxDuration)
}




sealed trait ObserverMessage
object GetProblemFromScene extends ObserverMessage
case class ProblemFromScene(anml: String) extends ObserverMessage
object ErrorOnProblemGeneration extends ObserverMessage

