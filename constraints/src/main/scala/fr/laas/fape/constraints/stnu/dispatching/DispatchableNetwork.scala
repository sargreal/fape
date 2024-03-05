package fr.laas.fape.constraints.stnu.dispatching

import scala.collection.JavaConverters._
import scala.collection.mutable
import fr.laas.fape.anml.model.concrete._
import fr.laas.fape.anml.pending.IntExpression
import fr.laas.fape.constraints.stnu.morris.DCMorris.{Lower, Req, Upper}
import fr.laas.fape.constraints.stnu.{InconsistentTemporalNetwork, STNU}
import fr.laas.fape.constraints.stnu.morris.{DCMorris, TemporalNetwork}
import fr.laas.fape.constraints.stnu.structurals.{DistanceMatrix, StnWithStructurals}
import fr.laas.fape.structures.{ISet, IList}

/**
  * The WaitConstraint class represents a constraint that forces the destination timepoint to be at least dist time units after the source timepoint.
  *
  * @param src
  * @param dst
  * @param dist
  * @param label
  */
class WaitConstraint(val src: TPRef, val dst: TPRef, val dist: Int, val label: TPRef) extends TemporalConstraint {
  override def usedVariables: Set[Variable] = Set(src, dst, label)
}

class DispatchableNetwork(val stn: StnWithStructurals) {
  import DistanceMatrix.plus

  // disable pseudo controllability checking as execution will provide updated information on the occurrence of
  // contingent points
  stn.shouldCheckPseudoControllability = false

  assert(stn.getStartTimePoint.nonEmpty, "A dispatchable STNU must have a temporal origin")

  /** Recorded WaitConstraints index by their sources and labels*/
  private val waitsBySource = mutable.Map[TPRef,mutable.ArrayBuffer[WaitConstraint]]()
  private val waitsByLabel  = mutable.Map[TPRef,mutable.ArrayBuffer[WaitConstraint]]()

  // record callback to propagate wait constraints on all updates of earliest start times
  stn.addEarliestExecutionUpdateListener(tp => {
    for(wait <- waitsByLabel.getOrElse(tp, Nil) ++ waitsBySource.getOrElse(tp, Nil))
      enforceWait(wait)
  })

  def addConstraint(constraint: TemporalConstraint): Unit = {
    constraint match {
      case c: MinDelayConstraint =>
        stn.addConstraint(c)
      case c: WaitConstraint =>
        waitsBySource.getOrElseUpdate(c.src, { mutable.ArrayBuffer() }) += c
        waitsByLabel.getOrElseUpdate(c.label, { mutable.ArrayBuffer() }) += c
        enforceWait(c)
      case c: ContingentConstraint =>
        stn.addMinDelay(c.src, c.dst, c.min.get)
        stn.addMaxDelay(c.src, c.dst, c.max.get)
    }
  }

  /** Adds a new minDelay constraint to ensure the given wait constraint is respected for the current earliest times */
  private def enforceWait(wait: WaitConstraint): Unit = {
    val est = Math.min(plus(stn.getEarliestTime(wait.src), wait.dist), stn.getEarliestTime(wait.label))
    stn.addMinDelay(stn.getStartTimePoint.get, wait.dst, est)
  }

  /** All timepoints that have been marked as executed */
  private val executions = mutable.Map[TPRef,Int]()

  /** Returns true if the given timepoint has previously been marked as executed */
  def isExecuted(tp: TPRef) = executions.contains(tp)

  /** Mark the timepoint as executed at hte given time */
  def setExecuted(tp: TPRef, time: Int): Unit = {
    executions += ((tp, time))
    stn.forceExecutionTime(tp, time)
  }

  private var currentTime = -1
  def getCurrentTime = currentTime

  /** Pushes back all non-executed timepoints to be after the given time */
  def setCurrentTime(time: Int): Unit = {
    if(time <= currentTime)
      return
    currentTime = time
    val start = stn.start.get
    for(tp <- stn.timepoints.asScala if !executions.contains(tp)) {
      if(tp.genre.isDispatchable) {
        if(stn.getMinDelay(start, tp) < time)
          stn.addMinDelay(start, tp, time)
      } else if (tp.genre.isContingent && stn.getEarliestTime(tp) < time) {
        // propagate this timepoint to all others
        for(o <- stn.timepoints.asScala if !executions.contains(o) && o.genre.isDispatchable) {
          if(stn.getMinDelay(tp, o) >= 0 && stn.getEarliestTime(o) <= time)
            stn.addMinDelay(start, o, time+1)
        }
        for(wait <- waitsByLabel.getOrElse(tp, Nil) ++ waitsBySource.getOrElse(tp, Nil)) {
          val est = Math.min(plus(stn.getEarliestTime(wait.src), wait.dist), time+1)
          stn.addMinDelay(start, wait.dst, est)
        }
      }
    }
  }

  /** Returns all timepoints that are executable for the given current time. */
  def getExecutables(currentTime: Int): ISet[TPRef] = {
    setCurrentTime(currentTime)
    // executables are all dispatchable that can be executed at the current time
    // println(stn.timepoints.asScala.toString)
    val executables = stn.timepoints.asScala
      .filter(_.genre.isDispatchable)
      // .filter(!isExecuted(_))
      .filter(stn.getEarliestTime(_) == currentTime)
    
    // println("recorded timepoints:"+ stn.timepointByIndex.toList.mkString(","))
    // println("dispatchables with earliestTime:"+ stn.timepoints.asScala.filter(_.genre.isDispatchable).map(tp => (tp,stn.getEarliestTime(tp))).mkString(","))
    // println("executables:"+ executables.mkString(","))

    // timepoints that are not executed yet
    val unexecutedPredecessors =
      (executables ++ stn.contingentLinks.map(c => c.dst))
        .filterNot(isExecuted(_))

    // println("contingents:"+ stn.contingentLinks.map(c => c.dst).toList.mkString(","))
    // println("unexecuted predecessors:"+ unexecutedPredecessors.mkString(","))

    // restrict executables to timepoints with not predecessor that is not executed yet
    val executablesWithNoExecutablePredecessors =
      executables.filter(tp => unexecutedPredecessors.forall(pred =>
        if(stn.getMinDelay(pred, tp) == 0)
          stn.getMaxDelay(pred, tp) == 0 // only executable if the two timepoints must be concurrent
        else
          true
      ))
    new ISet[TPRef](executablesWithNoExecutablePredecessors.toSet)
  }
}

object DispatchableNetwork {

  def getDispatchableNetwork[ID](stn: STNU, necessarilyObservableTimepoints: java.util.Set[TPRef]) : DispatchableNetwork =
    getDispatchableNetwork(stn, necessarilyObservableTimepoints.asScala.toSet)

  def getDispatchableNetwork[ID](stn: STNU, necessarilyObservableTimepoints: Set[TPRef]) : DispatchableNetwork = {
    // build network for DC checking
    val tn = TemporalNetwork.build(stn.getConstraintsWithoutStructurals.asScala, necessarilyObservableTimepoints, Set())
      .withoutInvisible
      .normalForm

    // check Dynamic Controllability
    val morris = new DCMorris()
    for(e <- tn.constraints)
      morris.addEdge(e)
    if(!morris.determineDC()._1)
      throw new InconsistentTemporalNetwork("Temporal network is not Dynamically Controllable")

    val timepointsFromIDs = stn.timepoints.asScala
      .filter(!_.genre.isStructural)
      .map(tp => (tp.id, tp))
      .toMap
    def tp(id: Int) = timepointsFromIDs(id)
    import IntExpression.lit

    // build the dispatchable network by extending the source STNU with all constraints infered by DC-Morris
    val dispatchableNetwork = new DispatchableNetwork(stn.asInstanceOf[StnWithStructurals])
    for(e <- morris.edges ++ morris.edgesForDispatchability if timepointsFromIDs.contains(e.from) && timepointsFromIDs.contains(e.to)) {
      e match {
        case Req(src, dst, d, _) =>
          dispatchableNetwork.addConstraint(new MinDelayConstraint(tp(dst), tp(src), lit(-d)))
        case Upper(src, dst, dist, label, _) =>
          dispatchableNetwork.addConstraint(new WaitConstraint(tp(dst), tp(src), -dist, tp(label)))
        case x:Lower =>
      }
    }
    dispatchableNetwork
  }
}
