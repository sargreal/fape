package fr.laas.fape.acting

import akka.actor.FSM
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import fr.laas.fape.acting.PlanningActor._
import fr.laas.fape.acting.actors.patterns.MessageLogger
import fr.laas.fape.acting.messages.{Event, GetPlan, NoPlanExists, PlanFound, PlannerMessage, PlannerReply, PlanningTimedOut}
import fr.laas.fape.planning.Planning
import fr.laas.fape.planning.core.planning.planner.Planner.EPlanState
import fr.laas.fape.planning.core.planning.planner.{Planner, PlanningOptions}
import fr.laas.fape.planning.core.planning.states.PartialPlan
import fr.laas.fape.planning.exceptions.PlanningInterruptedException
import fr.laas.fape.planning.util.TinyLogger

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

object PlanningActor {
  private def time = System.currentTimeMillis()
  val repairTime = 10000



  def apply(): Behavior[PlannerMessage] =
    idle()

  def idle():Behavior[PlannerMessage] = Behaviors.receiveMessage[PlannerMessage] {
    case GetPlan(initPlan, duration, reqID, replyTo) =>
      val options = new PlanningOptions()
      val planner = new Planner(initPlan, options)
      launchPlanningProcess(planner, duration, reqID, replyTo)
      planning(Some(planner))
  }
  def planning(prevPlanner: Option[Planner]): Behavior[PlannerMessage] = Behaviors.receiveMessage[PlannerMessage] { message =>
    (message, prevPlanner) match {
      case (GetPlan(initPlan, duration, reqID, replyTo), Some(previousPlanner)) =>
        previousPlanner.stopPlanning = true
        val planner = new Planner(initPlan, new PlanningOptions())
        launchPlanningProcess(planner, duration, reqID, replyTo)
        planning(Some(planner))
    }
  }

  def launchPlanningProcess(planner: Planner, duration: FiniteDuration, reqID: Int, replyTo: ActorRef[PlannerReply]): Unit = {
    Future {
      try {
        val solution = planner.search(time + duration.toMillis)
        if (solution != null) {
          replyTo ! PlanFound(solution, reqID)
          log.info("Got Plan")
        } else if(planner.planState == EPlanState.TIMEOUT) {
          replyTo ! PlanningTimedOut(reqID)
        } else {
          replyTo ! NoPlanExists(reqID)
        }
      } catch {
        case x: PlanningInterruptedException =>
          log.info(s"Planning interrupted ($reqID)")
      }
    }
  }
}
