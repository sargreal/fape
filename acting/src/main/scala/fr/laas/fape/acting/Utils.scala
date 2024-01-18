package fr.laas.fape.acting

import com.sun.org.apache.xpath.internal.operations.VariableSafeAbsRef
import fr.laas.fape.anml.model.AnmlProblem
import fr.laas.fape.anml.model.concrete._
import fr.laas.fape.anml.model.concrete.statements.Persistence
import fr.laas.fape.planning.core.planning.states.PartialPlan
import fr.laas.fape.planning.core.planning.states.modification.ChronicleInsertion

/**
  * Created by abitmonn on 11/23/16.
  */
object Utils {

  private var problem : AnmlProblem = null

  def setProblem(file: String): Unit = {
    problem = new AnmlProblem()
    problem.extendWithAnmlFile(file)
  }

  def getProblem = {
    require(problem != null)
    problem
  }

  def buildGoal(svName: String, args: List[String], value: String, deadline: Int = -1) = {
    assert(RefCounter.useGlobalCounter)
    val goal = new Chronicle
    val statement = new Persistence(
      problem.stateVariable(svName, args),
      problem.instance("true"),
      goal,
      RefCounter.getGlobalCounter)
    goal.addStatement(statement)
    if(deadline > -1) {
      goal.addConstraint(new MinDelayConstraint(statement.start, problem.start, -deadline))
    }
    new ChronicleInsertion(goal)
  }

  /**
    * Builds a task from a string of the form:
    *   name(arg1, arg2, ...)[deadline=deadline]
    * @param delayedTask
    * @return the task as a ChronicleInsertion
    */
  def buildTask(delayedTask: String): ChronicleInsertion = {
    val name = delayedTask.split("\\(")(0)
    val args = delayedTask.split("\\(")(1).split("\\)")(0).split(",").map(_.trim).toList
    // Deadline is optional
    val deadline = if(delayedTask.contains("@deadline")) {
      delayedTask.split("@deadline=")(1).split("]")(0).toInt
    } else {
      -1
    }
    buildTask(name, args, deadline)
  }

  def buildTask(name: String, args: List[String], deadline: Int = -1) = {
    assert(RefCounter.useGlobalCounter)
    val goal = new Chronicle
    val task = new Task("t-"+name, args.map(problem.instance(_)), None, problem.refCounter)
    goal.addTask(task)
    if(deadline > -1) {
      goal.addConstraint(new MinDelayConstraint(task.end, problem.start, -deadline))
    }
    new ChronicleInsertion(goal)
  }

  def asString(variable: VarRef, plan: PartialPlan) = {
    plan.domainOf(variable).get(0)
  }
}
