package fr.laas.fape.acting

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import fr.laas.fape.acting.messages._
import scopt.OParser
import scala.concurrent.duration._

import scala.collection.immutable
import scala.collection.mutable
import fr.laas.fape.planning.core.planning.states.modification.ChronicleInsertion
import fr.laas.fape.planning.util.TinyLogger
import fr.laas.fape.anml.model.concrete.MinDelayConstraint
import fr.laas.fape.anml.model.concrete.RefCounter
import akka.compat.Future
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import java.util.concurrent.CancellationException
import scala.util.Random
import akka.actor.typed.SupervisorStrategy
import akka.actor.typed.DeathPactException


object Acting {
  case class Config(
    problem: String = "",
    timeout: Int = 60,
    planningTimeout: Int = 10,
    interval: Int = 500,
    tasks: Seq[String] = Seq(),
    verbose: Int = 0,
    preparableTasks: Seq[String] = Seq(),
    planSelection: Seq[String] = Seq("dfs","ord-dec","soca"),
    flawSelection: Seq[String] = Seq("hier","ogf","abs","lcf","eogf")
  )
  
  val builder = OParser.builder[Config]
  val argParser = {
    import builder._
    OParser.sequence(
      programName("fape-acting"),
      head("fape-acting", "0.1"),
      help("help").text("prints this usage text"),
      arg[String]("<anml-problem>")
        .required()
        .action((x, c) => c.copy(problem = x))
        .text("path to the problem file"),
      opt[String]('t', "tasks")
        .optional()
        .valueName("task1(arg1, arg2, ...)[@deadline=<timepoint>][@delay=<timepoint>];...)")
        .action((x, c) => c.copy(tasks = x.split(";")))
        .text("tasks to execute"),
      opt[String]("timeout")
        .optional()
        .valueName("<time>")
        .action((x, c) => c.copy(timeout = x.toInt))
        .text("When to shutdown the system in seconds. Default=60"),
      opt[String]("planning-timeout")
        .optional()
        .valueName("<time>")
        .action((x, c) => c.copy(planningTimeout = x.toInt))
        .text("When to stop the planning in seconds. Default=10"),
      opt[String]('i',"interval")
        .optional()
        .valueName("<time>")
        .action((x, c) => c.copy(interval = x.toInt))
        .text("Interval between clock ticks in milliseconds. Default=500"),
      opt[Unit]('v',"verbose")
        .minOccurs(0)
        .maxOccurs(3)
        .action((_, c) => c.copy(verbose = c.verbose + 1))
        .text("increase verbosity of logging (0=WARNING, 1=INFO, 2=DEBUG, 3=TRACE)"),
      opt[Seq[String]]('p', "plan-selection")
        .optional()
        .valueName("<strategy1>,<strategy2>,...")
        .action((x, c) => c.copy(planSelection = x))
        .text("Plan selection strategies to use. Default=dfs,ord-dec,soca,robust"),
      opt[Seq[String]]('f', "flaw-selection")
        .optional()
        .valueName("<strategy1>,<strategy2>,...")
        .action((x, c) => c.copy(flawSelection = x))
        .text("Plan selection strategies to use. Default=hier,ogf,abs,lcf,eogf"),
      opt[Seq[String]]("preparable-tasks")
        .optional()
        .valueName("<taskName1>,<taskName2>,...")
        .action((x, c) => c.copy(preparableTasks = x))
        .text("Tasks that may be prepared at any point to optimize the plan. Default=none"),
    )
  }

  def parseArgs(args: Array[String]): Config = {
    OParser.parse(argParser, args, Config()) match {
      case Some(config) => config
      case _ => throw new IllegalArgumentException("Invalid arguments")
    }
  }

  def cancellableFuture[T](fun: Future[T] => T)(implicit ex: ExecutionContext): (Future[T], () => Boolean) = {
    val p = Promise[T]()
    val f = p.future
    p tryCompleteWith Future(fun(f))
    (f, () => p.tryFailure(new CancellationException))
  }

  def main(args: Array[String]): Unit = {
    RefCounter.useGlobalCounter = true
    Random.setSeed(0)
    val config = parseArgs(args)
    // Set logging level to debug if verbose is enabled
    setLoggingLevel(config.verbose)
    val system = ActorSystem(Acting(config), "fapeActing")
    
    // Shutdown the system after the timeout
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val (f, cancel) = cancellableFuture[Unit]( future => {
      try {
        Thread.sleep(config.timeout.seconds.toMillis)
        system.terminate()
      } catch {
        case _: CancellationException =>
      }
    })

    system.whenTerminated.onComplete { _ =>
      // Cancel the timeout if the system is terminated before the timeout
      cancel()
    }
    
  }

  private def setLoggingLevel(level: Int): Unit = {
    import ch.qos.logback.classic.{Level, Logger}
    import org.slf4j.LoggerFactory
    val root: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    val logLevel = level match {
      case 0 => Level.WARN
      case 1 => Level.INFO
      case 2 => Level.DEBUG
      case 3 => Level.TRACE
      case _ => Level.INFO
    }
    root.setLevel(logLevel)
    if (logLevel.levelInt <= Level.DEBUG_INT) {
      TinyLogger.logging = true
    } else {
      TinyLogger.logging = false
    }
  }

  val delayedTasks: mutable.Buffer[String] = mutable.Buffer()

  def apply(config: Config): Behavior[TimedReply] =  Behaviors
      .supervise(Behaviors.setup[TimedReply] { context =>
        Utils.setProblem(config.problem)
        Utils.setPlanningOptions(config.planSelection, config.flawSelection, config.preparableTasks)
        Utils.planningTimeout = config.planningTimeout
        val clock = context.spawn(Clock(config.interval), "clock")
        val manager = context.spawn(ActivityManager(clock), "manager")
        val submittedTasks = mutable.Buffer[String]()

        clock ! StopClock

        config.tasks.foreach { task =>
          if (Utils.getDelay(task) > 0) {
            delayedTasks.append(task)
            val index = delayedTasks.indexOf(task)
            clock ! ReplyAt(Utils.getDelay(task), index, context.self)
          } else {
            manager ! AddGoal(Utils.buildTask(task))
            submittedTasks.append(task)
          }
        }
        context.watch(manager)
        if (submittedTasks.size == config.tasks.size) {
          manager ! ShutdownAfterFinished
        }
        Behaviors.receiveSignal[TimedReply] {
          case (_, Terminated(_)) =>
            context.log.info("Shutting down the system")
            Behaviors.stopped
        }

        Behaviors.receiveMessage[TimedReply] {
          case TimedReply(timepoint, index) =>
            context.log.info("Adding task " + delayedTasks(index))
            val chronicle = Utils.buildTask(delayedTasks(index))
            manager ! AddGoal(chronicle)
            submittedTasks.append(delayedTasks(index))
            if (submittedTasks.size == config.tasks.size) {
              manager ! ShutdownAfterFinished
            }
            Behaviors.same
        }

    // Behaviors.receiveMessage[Tick] {
    //   case Tick(timepoint) =>
    //     // If all tasks have been executed, shutdown the system
    //     manager ! ExecutedGoals
    //     Behaviors.same
    // }
  }).onFailure[DeathPactException](SupervisorStrategy.stop)

}

