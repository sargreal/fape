package fr.laas.fape.acting

import fr.laas.fape.planning.core.planning.states.PartialPlan
import fr.laas.fape.planning.core.planning.planner.PlanningOptions
import fr.laas.fape.planning.core.planning.timelines.Timeline
import scala.collection.JavaConverters._
import fr.laas.fape.anml.model.ParameterizedStateVariable
import fr.laas.fape.planning.core.planning.states.Printer
import fr.laas.fape.anml.parser.AnmlParser
import fr.laas.fape.anml.model.SimpleType
import fr.laas.fape.anml.model.concrete.VarRef

class AnalysisResult(
    val totalTime: Number,
    val usedTime: Number,
    val usage: Number
)

class PlanAnalyzer(
    val plan: PartialPlan,
    val planningOptions: PlanningOptions,
    val currentTime: Int
) {

  /** Analyzes the usage of resources in the plan.
    */
  def analyze(): Map[VarRef, AnalysisResult] = {
    val groupedTimelines = getGroupedTimelines()

    val start = currentTime
    val end = plan.getEarliestStartTime(plan.pb.end)

    val usage = aggregateUsage(groupedTimelines, start, end)
    println("Resource usage for the upcoming actions:")
    for ((variable, analysisResult) <- usage) {
      println(
        f"  ${variable}%s : ${analysisResult.usedTime} / ${analysisResult.totalTime} = ${analysisResult.usage.doubleValue()}%2.2f"
      )
    }
    usage
  }

  def getInstanceUsage(instance: String): AnalysisResult = {
    val groupedTimelines = getGroupedTimelines(Some(instance))
    val usage = aggregateUsage(groupedTimelines, start=0)

    if (usage.size == 1) {
      return usage.head._2
    }
    println(
      s"Error: expected 1 timeline for instance $instance, found ${usage.size}"
    )
    return new AnalysisResult(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
  }

  def aggregateUsage(
      groupedTimelines: Map[VarRef, List[Timeline]],
      start: Int = currentTime,
      end: Int = plan.getEarliestStartTime(plan.pb.end)
  ): Map[VarRef, AnalysisResult] = {
    var result = Map[VarRef, AnalysisResult]()
    var totalAvailableTime = end - start
    for ((variable, timelines) <- groupedTimelines) {
      val stateVariable = timelines.head.stateVariable
      var usedTimes = List[(Int, Int)]()
      for (timeline <- timelines) {
        for (chainComponent <- timeline.chain) {
          for (logStatement <- chainComponent.statements) {
            val starttime = plan.getEarliestStartTime(logStatement.start)
            val endtime = plan.getEarliestStartTime(logStatement.end)
            usedTimes = usedTimes.+:(starttime, endtime)
          }
        }
      }
      usedTimes = usedTimes.sortWith((t1, t2) => t1._1 < t2._1)
      // Collect the intervals where the resource is used to compute the total time used
      var totalUsedTime = 0
      var lastEnd = start
      for ((s, e) <- usedTimes) {
        if (s < e) {
          if (e < lastEnd) {
            // do nothing
          } else if (s < lastEnd) {
            totalUsedTime += e - lastEnd
            lastEnd = e
          } else {
            totalUsedTime += e - s
            lastEnd = e
          }
        }
      }
      val usage = totalUsedTime.toDouble / totalAvailableTime.toDouble
      result += (variable -> new AnalysisResult(
        totalAvailableTime,
        totalUsedTime,
        usage
      ))
    }
    result
  }

  def getGroupedTimelines(
      singleInstance: Option[String] = None
  ): Map[VarRef, List[Timeline]] = {
    var groupedTimelines = Map[VarRef, List[Timeline]]()

    for (timeline <- plan.getTimelines.asScala) {
      for (instance <- timeline.stateVariable.args) {
        if (
          (singleInstance.isDefined && singleInstance.get.equals(
            instance.toString()
          )) ||
          (singleInstance.isEmpty && isRelevantInstance(instance))
        ) {
          val list = groupedTimelines.getOrElse(instance, List[Timeline]())
          groupedTimelines = groupedTimelines + (instance -> (timeline :: list))
        }
      }
    }
    groupedTimelines
  }

  def isRelevantInstance(variable: VarRef): Boolean = {
    planningOptions.resourceTypes.asScala.exists(resourceTypeString => {
      val resourceType = new SimpleType(resourceTypeString, None)
      variable.typ.compatibleWith(resourceType)
    })
  }

}
