package fr.laas.fape.acting.actors.patterns

import akka.actor.Actor
import akka.event.Logging
import fr.laas.fape.acting.messages.Tick

trait MessageLogger extends Actor {

  val log = Logging(context.system, this)

  override def aroundReceive(receive: Actor.Receive, msg: Any): Unit = {
    msg match {
      case Tick =>
      case _ => log.info(s"From: [${sender().path}] --> $msg")
    }
    super.aroundReceive(receive, msg)
  }
}
