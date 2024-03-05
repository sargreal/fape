package fr.laas.fape.acting

import com.sun.org.apache.xpath.internal.operations.VariableSafeAbsRef
import fr.laas.fape.anml.model.AnmlProblem
import fr.laas.fape.anml.model.concrete._
import fr.laas.fape.anml.model.concrete.statements.Persistence
import fr.laas.fape.planning.core.planning.states.PartialPlan
import fr.laas.fape.planning.core.planning.states.modification.ChronicleInsertion
import fr.laas.fape.constraints.stnu.morris.Parser
import fr.laas.fape.planning.core.planning.planner.PlanningOptions
import scala.collection.JavaConverters._

/** Created by abitmonn on 11/23/16.
  */
object Utils {

  private var problem: AnmlProblem = null

  private var planningOptions = new PlanningOptions(
    List("dfs", "robust", "ord-dec", "soca").asJava,
    List("hier", "ogf", "abs", "lcf", "eogf").asJava
  )

  var planningTimeout = 10

  def getPlanningOptions: PlanningOptions = planningOptions
  def setPlanningOptions(
      planSelStrategies: Seq[String],
      flawSelStrategies: Seq[String],
      preparableTasks: Seq[String]
  ): Unit = {
    planningOptions =
      new PlanningOptions(planSelStrategies.asJava, flawSelStrategies.asJava)
    planningOptions.preparableTasks = preparableTasks.asJava
  }

  def setProblem(file: String): Unit = {
    problem = new AnmlProblem()
    problem.extendWithAnmlFile(file)
  }

  def getProblem = {
    require(problem != null)
    problem
  }

  def buildGoal(
      svName: String,
      args: List[String],
      value: String,
      deadline: Int = -1
  ) = {
    assert(RefCounter.useGlobalCounter)
    val goal = new Chronicle
    goal.origin = "state-goal"
    val statement = new Persistence(
      problem.stateVariable(svName, args),
      problem.instance("true"),
      goal,
      RefCounter.getGlobalCounter
    )
    goal.addStatement(statement)
    if (deadline > -1) {
      goal.addConstraint(
        new MinDelayConstraint(statement.start, problem.start, -deadline)
      )
    }
    new ChronicleInsertion(goal)
  }

  def getDeadline(task: String) = {
    if (task.contains("@deadline")) {
      task.split("@deadline=")(1).split("@")(0).toInt
    } else {
      -1
    }
  }

  def getDelay(task: String) = {
    if (task.contains("@delay=")) {
      task.split("@delay=")(1).split("@")(0).toInt
    } else {
      0
    }
  }

  def getStart(task: String) = {
    if (task.contains("@start=")) {
      task.split("@start=")(1).split("@")(0).toInt
    } else {
      0
    }
  }

  /** Builds a task from a string of the form: name(arg1, arg2,
    * ...)@deadline=deadline@delay=delay
    *
    * Delay is not interpreted here, but can be parsed using getDelay
    * @param task
    * @return
    *   the task as a ChronicleInsertion
    */
  def buildTask(task: String): ChronicleInsertion = {
    val name = task.split("\\(")(0)
    val args =
      task.split("\\(")(1).split("\\)")(0).split(",").map(_.trim).toList
    // Deadline is optional
    val delay = getDelay(task)
    val deadline = getDeadline(task)
    val start = getStart(task)
    buildTask(name, args, deadline, if (start > delay) start else delay)
  }

  def buildTask(
      name: String,
      args: List[String],
      deadline: Int = -1,
      delay: Int = -1
  ) = {
    assert(RefCounter.useGlobalCounter)
    val goal = new Chronicle
    goal.origin = "task-goal"
    val task = new Task(
      "t-" + name,
      args.map(problem.instance(_)),
      None,
      problem.refCounter
    )
    goal.addTask(task)
    if (deadline > -1) {
      goal.addConstraint(
        new MinDelayConstraint(task.end, problem.start, -deadline)
      )
    }
    if (delay > -1) {
      val constraint = new MinDelayConstraint(problem.start, task.start, delay)
      goal.addConstraint(constraint)
    }
    new ChronicleInsertion(goal)
  }

  def asString(variable: VarRef, plan: PartialPlan) = {
    plan.domainOf(variable).get(0)
  }

  def setExecuting(act: Action) {
    act.setStatus(ActionStatus.EXECUTING)
    if (act.hasParent) {
      setExecuting(act.parent)
    }
  }

  /** Sets the status of the action and all its parents to EXECUTED if all
    * siblings have been executed as well.
    *
    * @param act
    * @param plan
    */
  def setExecuted(act: Action, plan: PartialPlan) {
    plan
      .getAllActions()
      .stream
      .filter(_.hasParent)
      .filter(_.parent == act)
      .filter(_.status != ActionStatus.EXECUTED)
      .count() match {
      case 0 =>
        act.setStatus(ActionStatus.EXECUTED)
        if (act.hasParent) {
          setExecuted(act.parent, plan)
        }
      case _ =>
    }
  }
}
