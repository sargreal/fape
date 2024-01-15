package fr.laas.fape.acting

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}

import scala.collection.immutable


object Acting {

  def apply(): Behavior[NotUsed] =
    Behaviors.setup { context =>
      val activityManager = context.spawn(ActivityManager(), "activity-manager")
      context.watch(activityManager)

      Behaviors.receiveSignal {
        case (_, Terminated(_)) =>
          Behaviors.stopped
      }
    }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) throw new IllegalArgumentException("initial problem file required")
    Utils.setProblem(args(0))
    val system = (Acting(), "fapeActing")
  }

}

