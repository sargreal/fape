package fr.laas.fape.acting

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Terminated}
import fr.laas.fape.acting.messages.AddGoal
import scopt.OParser
import scala.concurrent.duration._

import scala.collection.immutable


object Acting {
  case class Config(
    problem: String = "",
    timeout: Int = 60,
    tasks: Seq[String] = Seq(),
    delayedTasks: Seq[String] = Seq(),
    verbose: Boolean = false
  )
  
  val builder = OParser.builder[Config]
  val argParser = {
    import builder._
    OParser.sequence(
      programName("fape-acting"),
      head("fape-acting", "0.1"),
      help("help").text("prints this usage text"),
      opt[String]('p', "problem")
        .valueName("<domain>.<problem>.pb.anml")
        .required()
        .action((x, c) => c.copy(problem = x))
        .text("path to the problem file"),
      opt[String]('t', "tasks")
        .optional()
        .valueName("task1(arg1, arg2, ...)[@deadline=<timepoint>];...)")
        .action((x, c) => c.copy(tasks = x.split(";")))
        .text("tasks to execute"),
      opt[String]("timeout")
        .optional()
        .valueName("<time>")
        .action((x, c) => c.copy(timeout = x.toInt))
        .text("timeout in seconds"),
      opt[Unit]("verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("enable debug logging"),
    )
  }

  def parseArgs(args: Array[String]): Config = {
    OParser.parse(argParser, args, Config()) match {
      case Some(config) => config
      case _ => throw new IllegalArgumentException("Invalid arguments")
    }
  }

  def main(args: Array[String]): Unit = {
    val config = parseArgs(args)
    // Set logging level to debug if verbose is enabled
    if(config.verbose) {
      setLoggingLevel("DEBUG")
    } else {
      setLoggingLevel("INFO")
    }
    Utils.setProblem(config.problem)
    val system = ActorSystem(ActivityManager(), "fapeActing")
    config.tasks.foreach { task =>
      system ! AddGoal(Utils.buildTask(task))
    }
    Thread.sleep(config.timeout * 1000)
    system.terminate()
    
  }

  private def setLoggingLevel(level: String): Unit = {
    import ch.qos.logback.classic.{Level, Logger}
    import org.slf4j.LoggerFactory
    val root: Logger = LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger]
    root.setLevel(Level.toLevel(level))
  }

}

