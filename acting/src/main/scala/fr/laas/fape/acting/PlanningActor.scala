package fr.laas.fape.acting

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import fr.laas.fape.acting.messages._
import fr.laas.fape.planning.core.planning.planner.Planner.EPlanState
import fr.laas.fape.planning.core.planning.planner.{Planner, PlanningOptions}
import fr.laas.fape.planning.exceptions.PlanningInterruptedException

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration


object PlanningActor {
  private def time = System.currentTimeMillis()

  val repairTime = 10000

  def apply(): Behavior[PlannerEvent] =
    idle()

  def idle(): Behavior[PlannerEvent] = Behaviors.setup { context =>
    Behaviors.receiveMessage[PlannerEvent] {
      case GetPlan(initPlan, duration, reqID, replyTo) =>
        val planner = new Planner(initPlan, Utils.getPlanningOptions)
        launchPlanningProcess(planner, duration, reqID, replyTo, context)
        planning(Some(planner))
    }
  }

  def planning(prevPlanner: Option[Planner]): Behavior[PlannerEvent] = Behaviors.setup { context =>
    Behaviors.receiveMessage[PlannerEvent] { message =>
      (message, prevPlanner) match {
        case (GetPlan(initPlan, duration, reqID, replyTo), Some(previousPlanner)) =>
          previousPlanner.stopPlanning = true
          val planner = new Planner(initPlan, Utils.getPlanningOptions)
          launchPlanningProcess(planner, duration, reqID, replyTo, context)
          planning(Some(planner))
      }
    }
  }

  def launchPlanningProcess(planner: Planner, duration: FiniteDuration, reqID: Int, replyTo: ActorRef[PlannerReply], context: ActorContext[PlannerEvent]): Unit = {
    Future {
      try {
        val solution = planner.search(time + duration.toMillis)
        if (solution != null) {
          replyTo ! PlanFound(solution, reqID)
        } else if (planner.planState == EPlanState.TIMEOUT) {
          replyTo ! PlanningTimedOut(reqID)
        } else {
          replyTo ! NoPlanExists(reqID)
        }
      } catch {
        case x: PlanningInterruptedException =>
          println(s"Planning interrupted ($reqID)")
          replyTo ! PlanningTimedOut(reqID)
        case x: Throwable =>
          println(s"Error while planning ($reqID)")
          x.printStackTrace()
          replyTo ! NoPlanExists(reqID)
      }
    }
  }
}
