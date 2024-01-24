package fr.laas.fape.acting

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import fr.laas.fape.acting.messages.{
  ActiveTimepointNotification,
  DispatchEvent,
  ExecutionRequest
}
import fr.laas.fape.anml.model.concrete.{ActionStatus, Action}
import akka.actor.typed.scaladsl.TimerScheduler
import fr.laas.fape.acting.messages.{TimepointActive, ClockEvent}
import scala.concurrent.duration._
import fr.laas.fape.acting.messages.DispatchSuccess
import fr.laas.fape.anml.model.concrete.ActRef
import fr.laas.fape.planning.core.planning.states.Printer
import fr.laas.fape.planning.core.planning.states.PartialPlan
import akka.actor.typed.ActorRef
import fr.laas.fape.acting.messages.TimedReply
import fr.laas.fape.acting.messages.ReplyAt

object DispatchActor {
  sealed trait MData
  final case class MExecutingRequests(requests: Map[ActRef, ExecutionRequest], clock: ActorRef[ClockEvent]) extends MData

  
  def apply(clock: ActorRef[ClockEvent]): Behavior[DispatchEvent] = Behaviors.setup { context =>
    executing(MExecutingRequests(Map(), clock))
  }

  def executing(data: MExecutingRequests): Behavior[DispatchEvent] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[DispatchEvent] {
        case req: ExecutionRequest =>
          context.log.debug(s"Executing action ${Printer.action(req.plan,req.action)}")
          setExecuting(req.action)
          val endTime = req.plan.getEarliestStartTime(req.action.end)
          context.log.debug(s"Expected finish at t=$endTime")
          data.clock ! ReplyAt(endTime, req.action.id.toInt, context.self)
          executing(MExecutingRequests(data.requests + (req.action.id -> req), data.clock))
        case TimedReply(timepoint, reference) =>
          data.requests.get(new ActRef(reference)) match {
            case Some(req) =>
              context.log.debug(
                s"[$timepoint] Action ${Printer.action(req.plan,req.action)} executed"
              )
              setExecuted(req.action, req.plan)
              req.actorRef ! DispatchSuccess(req.action, timepoint)
              executing(MExecutingRequests(data.requests - req.action.id, data.clock))
            case None =>
              context.log.warn(
                s"[$timepoint] Action $reference is not in the list of executing actions"
              )
              Behaviors.same
          }
        case ActiveTimepointNotification(action, timepointActive) =>
          data.requests.get(action) match {
            case Some(req) =>
              val t = req.plan.getEarliestStartTime(timepointActive.tp)
              timepointActive.tp.id match {
                case req.action.end.id =>
                  setExecuted(req.action, req.plan)
                  context.log.debug(
                    s"[$t] Action ${Printer.action(req.plan,req.action)} executed"
                  )
                  req.actorRef ! DispatchSuccess(req.action, t)
                  executing(MExecutingRequests(data.requests - action, data.clock))
                case _ =>
                  context.log.warn(
                    s"[$t] Action ${Printer.action(req.plan,req.action)} found, but timpoint ${timepointActive.tp} is not the end"
                  )
                  Behaviors.same
              }
            case None =>
              context.log.warn(
                s"Action $action is not in the list of executing actions"
              )
              Behaviors.same
          }
      }
    }

    def setExecuting(act: Action) {
      act.setStatus(ActionStatus.EXECUTING)
      if (act.hasParent) {
        setExecuting(act.parent)
      }
    }

    /**
      * Sets the status of the action and all its parents to EXECUTED if all siblings have been executed as well.
      *
      * @param act
      * @param plan
      */
    def setExecuted(act: Action, plan: PartialPlan) {
      plan.getAllActions().stream.filter(_.hasParent).filter(_.parent == act).filter(_.status != ActionStatus.EXECUTED).count() match {
        case 0 =>
          act.setStatus(ActionStatus.EXECUTED)
          if (act.hasParent) {
            setExecuted(act.parent, plan)
          }
        case _ =>
      }
    }
}
