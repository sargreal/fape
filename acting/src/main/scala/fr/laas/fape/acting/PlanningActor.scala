package fr.laas.fape.acting

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import fr.laas.fape.acting.messages._
import fr.laas.fape.planning.core.planning.planner.Planner.EPlanState
import fr.laas.fape.planning.core.planning.planner.{Planner, PlanningOptions}
import fr.laas.fape.planning.exceptions.PlanningInterruptedException
import scala.collection.JavaConverters._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import fr.laas.fape.planning.core.planning.states.PartialPlan
import scala.concurrent.ExecutionContext
import scala.util.Random
import fr.laas.fape.anml.parser.PType
import fr.laas.fape.anml.model.Type

object PlanningActor {
  private def time = System.currentTimeMillis()

  val repairTime = 10000

  def apply(): Behavior[PlannerEvent] =
    idle()

  def idle(): Behavior[PlannerEvent] = Behaviors.setup { context =>
    Behaviors.receiveMessage[PlannerEvent] {
      case GetPlan(
            initPlan,
            duration,
            currentTime,
            reqID,
            replyTo,
            previousUnoptimizedPlan
          ) =>
        val planner = new Planner(initPlan, Utils.getPlanningOptions)
        launchPlanningProcess(
          planner,
          duration,
          currentTime,
          reqID,
          replyTo,
          context.self,
          previousUnoptimizedPlan
        )
        planning(Some(planner))
    }
  }

  def planning(prevPlanner: Option[Planner]): Behavior[PlannerEvent] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage[PlannerEvent] { message =>
        (message, prevPlanner) match {
          case (
                GetPlan(
                  initPlan,
                  duration,
                  currentTime,
                  reqID,
                  replyTo,
                  previousUnoptimizedPlan
                ),
                Some(previousPlanner)
              ) =>
            previousPlanner.stopPlanning = true
            val planner = new Planner(initPlan, Utils.getPlanningOptions)
            launchPlanningProcess(
              planner,
              duration,
              currentTime,
              reqID,
              replyTo,
              context.self,
              previousUnoptimizedPlan
            )
            planning(Some(planner))
        }
      }
    }

  def launchPlanningProcess(
      planner: Planner,
      duration: FiniteDuration,
      currentTime: Int,
      reqID: Int,
      replyTo: ActorRef[PlannerReply],
      self: ActorRef[PlannerEvent],
      previousUnoptimizedPlan: Option[PartialPlan]
  ): Future[Unit] = {
    Future {
      try {
        val solution = planner.search(time + duration.toMillis)
        if (solution != null) {
          optimizePlan(solution, Utils.getPlanningOptions, currentTime) match {
            case Some(optPlan) =>
              self ! GetPlan(
                optPlan,
                duration,
                currentTime,
                reqID,
                replyTo,
                Some(solution)
              )
            case None =>
              replyTo ! PlanFound(solution, reqID)
          }
        } else {
          if (previousUnoptimizedPlan.isDefined) {
            println("No plan found, returning previous plan")
            replyTo ! PlanFound(previousUnoptimizedPlan.get, reqID)
          } else if (planner.planState == EPlanState.TIMEOUT && !planner.stopPlanning) {
            replyTo ! PlanningTimedOut(reqID)
          } else {
            replyTo ! NoPlanExists(reqID)
          }
        }
      } catch {
        case x: PlanningInterruptedException =>
          println(s"Planning interrupted ($reqID)")
          if (previousUnoptimizedPlan.isDefined) {
            replyTo ! PlanFound(previousUnoptimizedPlan.get, reqID)
          } else if (!planner.stopPlanning) {
            replyTo ! PlanningTimedOut(reqID)
          }
        case x: Throwable =>
          println(s"Error while planning ($reqID)")
          x.printStackTrace()
          if (previousUnoptimizedPlan.isDefined) {
            replyTo ! PlanFound(previousUnoptimizedPlan.get, reqID)
          } else {
            replyTo ! NoPlanExists(reqID)
          }
      }
    }
  }

  def optimizePlan(
      plan: PartialPlan,
      options: PlanningOptions,
      currentTime: Int
  ): Option[PartialPlan] = {
    val preparableTasks = Utils.getPlanningOptions.preparableTasks
    var result: Option[PartialPlan] = None
    if (!preparableTasks.isEmpty()) {
      val planAnalyzer = new PlanAnalyzer(plan, options, currentTime)
      val usages = planAnalyzer.analyze()
      for ((variable, analysisResult) <- usages) {
        if (analysisResult.usage.doubleValue() < 1) {
          // Select a random task to add to the plan
          val taskName =
            preparableTasks.get(Random.nextInt(preparableTasks.size))
          // Find unused instances of the required type for the task
          val variableTypes = plan.pb.tasks.get(taskName) match {
            case Some(args) => args.map(a => plan.pb.instances.asType(a.tipe))
            case None       => List[Type]()
          }
          val instances = variableTypes.map(t =>
            plan.pb.instances.instancesOfType(t.name).asScala
          )
          val unusedInstances = instances.map(is =>
            is.filter(i =>
              planAnalyzer.getInstanceUsage(i).usedTime.intValue == 0
            )
          )
          if (!unusedInstances.exists(_.isEmpty)) {
            // Create a new plan with the task and the first possible instances as arguments
            
            var consistent = false
            var newPlan = plan.cc(false)
            var newArgs = List[String]()
            var deadline = 0

            for (combination <- combinationsIterator(unusedInstances.map(_.toList))) {
              if (!consistent) {
                var newPlan = plan.cc(false)
                newArgs = combination
                deadline = plan.getEarliestStartTime(plan.pb.end)
                val consistent = newPlan.apply(
                  Utils.buildTask(
                    taskName,
                    newArgs,
                    delay = currentTime,
                    deadline = plan.getEarliestStartTime(plan.pb.end)
                  ),
                  false
                )
              }
            }
            if (consistent) {
              println(
                "Inserting new prepared task " + taskName + "(" + newArgs
                  .mkString(
                    ","
                  ) + ")@delay=" + currentTime + "@deadline=" + deadline
              )
              result = Some(newPlan)
            }
          }
        }
      }
    }
    return result
  }

  def combinationsIterator(instances: List[List[String]]): Iterator[List[String]] = {
    instances match {
      case Nil => Iterator(Nil)
      case head :: tail =>
        for {
          h <- head.iterator
          t <- combinationsIterator(tail)
        } yield h :: t
    }
  }
}
