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
import fr.laas.fape.acting.messages.TimepointActive
import scala.concurrent.duration._
import fr.laas.fape.acting.messages.DispatchSuccess
import fr.laas.fape.anml.model.concrete.ActRef
import fr.laas.fape.planning.core.planning.states.Printer

object DispatchActor {
  sealed trait MData
  final case class MExecutingRequests(requests: Map[ActRef, ExecutionRequest]) extends MData

  def apply(): Behavior[DispatchEvent] = Behaviors.setup { context =>
    Behaviors.withTimers(timers => new DispatchActor(timers).executing(MExecutingRequests(Map())))
  }

}

import fr.laas.fape.acting.DispatchActor._

class DispatchActor(timers: TimerScheduler[DispatchEvent]) {

  def t = Clock.time

  // def idle(): Behavior[DispatchEvent] = Behaviors.setup { context =>
  //   Behaviors.receiveMessage[DispatchEvent] {
  //     case req: ExecutionRequest(action, plan, replyTo) =>
  //       context.log.info(s"[$t] Executing action ${action.name}")
  //       action.setStatus(ActionStatus.EXECUTING)
  //       timers.startSingleTimer(
  //         action.name,
  //         ActiveTimepointNotification(action.name, TimepointActive(action.end)),
  //         Clock.toMilliDur(
  //           plan.getEarliestStartTime(action.end)
  //         ) - Clock.timeMilliDur
  //       )
  //       executing(MExecutingRequests(Map(action.name -> req)))
  //     case ActiveTimepointNotification(actionName, timepointActive) =>
  //       context.log.info(s"[$t] Have not executed aything yet")
  //       Behaviors.same
  //   }
  // }

  def executing(data: MExecutingRequests): Behavior[DispatchEvent] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[DispatchEvent] {
        case req: ExecutionRequest =>
          context.log.info(s"[$t] Executing action ${Printer.action(req.plan,req.action)}")
          req.action.setStatus(ActionStatus.EXECUTING)
          timers.startSingleTimer(
            req.action.id,
            ActiveTimepointNotification(
              req.action.id,
              TimepointActive(req.action.end)
            ),
            Clock.toMilliDur(
              req.plan.getEarliestStartTime(req.action.end)
            ) - Clock.timeMilliDur
          )
          executing(MExecutingRequests(data.requests + (req.action.id -> req)))
        case ActiveTimepointNotification(action, timepointActive) =>
          data.requests.get(action) match {
            case Some(req) =>
              timepointActive.tp.id match {
                case req.action.end.id =>
                  req.action.setStatus(ActionStatus.EXECUTED)
                  context.log.info(
                    s"[$t] Action ${Printer.action(req.plan,req.action)} executed"
                  )
                  req.actorRef ! DispatchSuccess(req.action)
                  executing(MExecutingRequests(data.requests - action))
                case _ =>
                  context.log.info(
                    s"[$t] Action ${Printer.action(req.plan,req.action)} found, but timpoint ${timepointActive.tp} is not the end"
                  )
                  Behaviors.same
              }
            case None =>
              context.log.info(
                s"[$t] Action $action is not in the list of executing actions"
              )
              Behaviors.same
          }
      }
    }
}
