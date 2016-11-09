package fr.laas.fape.constraints.stnu.structurals

import fr.laas.fape.anml.model.concrete.{ContingentConstraint, MinDelayConstraint, TPRef, TemporalConstraint}
import fr.laas.fape.anml.pending.IntExpression
import fr.laas.fape.constraints.stn.{DistanceGraphEdge, STN}
import fr.laas.fape.constraints.stnu.{Constraint, Controllability, InconsistentTemporalNetwork, STNU}
import fr.laas.fape.constraints.stnu.parser.STNUParser
import planstack.graph.core.LabeledEdge
import planstack.graph.printers.NodeEdgePrinter
import planstack.structures.IList

import scala.collection.mutable

object StnWithStructurals {

  var debugging = false

  val INF: Int = Int.MaxValue /2 -1 // set to avoid overflow on addition of int values
  val NIL: Int = 0

  def buildFromString(str: String) : StnWithStructurals[String] = {
    val stn = new StnWithStructurals[String]()
    val parser = new STNUParser
    parser.parseAll(parser.problem, str) match {
      case parser.Success((tps,constraints, optStart, optEnd),_) => {
        for(tp <- tps) {
          stn.recordTimePoint(tp)
        }
        optStart match {
          case Some(start) => stn.setStart(start)
          case None =>
        }
        optEnd match {
          case Some(end) => stn.setEnd(end)
          case None =>
        }
        for(constraint <- constraints) {
          stn.addConstraint(constraint)
        }
      }
    }
    stn
  }
}

import StnWithStructurals._

class StnWithStructurals[ID](val nonRigidIndexes: mutable.Map[TPRef,Int],
                             val timepointByIndex: mutable.ArrayBuffer[TPRef],
                             var dist: DistanceMatrix,
                             val rigidRelations: RigidRelations,
                             val contingentLinks: mutable.ArrayBuffer[ContingentConstraint],
                             var optStart: Option[TPRef],
                             var optEnd: Option[TPRef],
                             var originalEdges: List[DistanceGraphEdge],
                             var consistent: Boolean
                            )
  extends STNU[ID] with DistanceMatrixListener {

  def this() = this(mutable.Map(), mutable.ArrayBuffer(), new DistanceMatrix(), new RigidRelations(), mutable.ArrayBuffer(), None, None, Nil, true)

  override def clone() : StnWithStructurals[ID] = new StnWithStructurals[ID](
    nonRigidIndexes.clone(), timepointByIndex.clone(), dist.clone(), rigidRelations.clone(), contingentLinks.clone(),
    optStart, optEnd, originalEdges, consistent
  )

  dist.listeners += this

  def timepoints = new IList[TPRef]((nonRigidIndexes.keySet ++ rigidRelations._anchorOf.keySet).toList)

  private def toIndex(tp:TPRef) : Int = nonRigidIndexes(tp)

  private def isKnown(tp: TPRef) = nonRigidIndexes.contains(tp) || rigidRelations.isAnchored(tp)

  override def recordTimePoint(tp: TPRef): Int = {
    assert(!isKnown(tp))
    val id = dist.createNewNode()
    nonRigidIndexes.put(tp, id)
    rigidRelations.addAnchor(tp)
    while(timepointByIndex.size <= id) {
      timepointByIndex.append(null)
    }
    assert(timepointByIndex(id) == null)
    timepointByIndex(id) = tp
    optEnd match {
      case Some(end)  => enforceMinDelay(tp, end, 0)
      case None =>
    }
    id
  }

  def addMinDelay(from:TPRef, to:TPRef, minDelay:Int) =
    addEdge(to, from, -minDelay)

  def addMaxDelay(from: TPRef, to: TPRef, maxDelay: Int) =
    addMinDelay(to, from, -maxDelay)

  private def addEdge(a:TPRef, b :TPRef, t:Int): Unit = {
    originalEdges = new DistanceGraphEdge(a, b, t) :: originalEdges
    if(!isKnown(a))
      recordTimePoint(a)
    if(!isKnown(b))
      recordTimePoint(b)

    val (aRef:TPRef, aToRef:Int) =
      if(rigidRelations.isAnchored(a))
        (rigidRelations._anchorOf(a), rigidRelations.distFromAnchor(a))
      else
        (a, 0)
    val (bRef:TPRef, refToB) =
      if(rigidRelations.isAnchored(b))
        (rigidRelations._anchorOf(b), rigidRelations.distToAnchor(b))
      else (b, 0)
    dist.enforceDist(toIndex(aRef), toIndex(bRef), aToRef + t + refToB)
  }

  def addConstraint(c: TemporalConstraint): Unit = {
    c match {
      case req: MinDelayConstraint if req.minDelay.isKnown =>
        addMinDelay(req.src, req.dst, req.minDelay.get)
      case cont: ContingentConstraint if cont.min.isKnown && cont.max.isKnown =>
        addMinDelay(cont.src, cont.dst, cont.min.get)
        addMaxDelay(cont.src, cont.dst, cont.max.get)
        contingentLinks.append(cont)
      case _ =>
        throw new RuntimeException("Constraint: "+c+" is not properly supported")
    }
  }

  private def rigidAwareDist(a:TPRef, b:TPRef) : Int = {
    val (aRef:TPRef, aToRef:Int) =
      if(rigidRelations.isAnchored(a))
        (rigidRelations._anchorOf(a), rigidRelations.distToAnchor(a))
      else
        (a, 0)
    val (bRef:TPRef, refToB) =
      if(rigidRelations.isAnchored(b))
        (rigidRelations._anchorOf(b), rigidRelations.distFromAnchor(b))
      else (b, 0)

    val refAToRefB = distanceBetweenNonRigid(aRef, bRef)
    DistanceMatrix.plus(aToRef, DistanceMatrix.plus(refAToRefB, refToB))
  }

  private def distanceBetweenNonRigid(a: TPRef, b: TPRef) = {
    dist.getDistance(toIndex(a), toIndex(b))
  }

  def concurrent(tp1: TPRef, tp2: TPRef) = rigidAwareDist(tp1,tp2) == rigidAwareDist(tp2,tp1)

  private def minDelay(from: TPRef, to:TPRef) = -rigidAwareDist(to, from)
  private def maxDelay(from: TPRef, to: TPRef) = rigidAwareDist(from, to)
  private def beforeOrConcurrent(first: TPRef, second: TPRef) = rigidAwareDist(second, first) <= NIL
  private def strictlyBefore(first: TPRef, second: TPRef) = rigidAwareDist(second, first) < NIL
  private def between(tp: TPRef, min:TPRef, max:TPRef) = beforeOrConcurrent(min, tp) && beforeOrConcurrent(tp, max)
  private def strictlyBetween(tp: TPRef, min:TPRef, max:TPRef) = strictlyBefore(min, tp) && strictlyBefore(tp, max)

  override def distanceUpdated(a: Int, b: Int): Unit = {
    // check if the network is now inconsistent
    if (dist.getDistance(a, b) + dist.getDistance(b, a) < 0) {
      if(debugging)
        assert(!consistencyWithBellmanFord(), "Problem with the consistency of the STN")
      consistent = false
      throw new InconsistentTemporalNetwork
    }

    if (a == b)
      return

    // if there is a structural timepoint rigidly fixed to another, record this relation and simplify
    // the distance matrix
    if(dist.getDistance(a,b) == -dist.getDistance(b,a)) {
      val originalDist = dist.getDistance(a, b)
      val tpA = timepointByIndex(a)
      val tpB = timepointByIndex(b)
      assert(!rigidRelations.isAnchored(tpA))
      assert(!rigidRelations.isAnchored(tpB))
      if(tpA.genre.isStructural || tpB.genre.isStructural) {
        // record rigid relation
        rigidRelations.addRigidRelation(tpA, tpB, dist.getDistance(a, b))

        val (anchored, anchor) =
          if(rigidRelations.isAnchored(tpA)) (tpA, tpB)
          else if(rigidRelations.isAnchored(tpB)) (tpB,tpA)
          else throw new RuntimeException("No timepoint is considered as anchored after recording a new rigid relation")

        // remove the anchored timepoint from distance matrix
        dist.compileAwayRigid(toIndex(anchored), toIndex(anchor))
        timepointByIndex(toIndex(anchored)) = null
        nonRigidIndexes.remove(anchored)
        assert(originalDist == rigidAwareDist(tpA, tpB))
      }
    }
  }

  /** Makes an independent clone of this STN. */
  override def deepCopy(): StnWithStructurals[ID] = clone()

  /** Record this time point as the global start of the STN */
  override def recordTimePointAsStart(tp: TPRef): Int = {
    if(!isKnown(tp))
      recordTimePoint(tp)
    setStart(tp)
    nonRigidIndexes(tp)
  }

  def setStart(start: TPRef): Unit = {
    assert(isKnown(start))
    assert(optStart.isEmpty || optStart.get == start)
    optStart = Some(start)
    optEnd match {
      case Some(end) => enforceMinDelay(start, end, 0)
      case None =>
    }
  }

  /** Unifies this time point with the global end of the STN */
  override def recordTimePointAsEnd(tp: TPRef): Int = {
    if(!isKnown(tp))
      recordTimePoint(tp)
    setEnd(tp)
    nonRigidIndexes(tp)
  }

  def setEnd(end: TPRef): Unit = {
    assert(isKnown(end))
    assert(optEnd.isEmpty || optEnd.get == end)
    optEnd = Some(end)
    for(tp <- timepoints.asScala) {
      enforceBefore(tp, end)
    }
    optStart match {
      case Some(start) => enforceMinDelay(start, end, 0)
      case None =>
    }
  }

  /** Returns true if the STN is consistent (might trigger a propagation */
  override def isConsistent(): Boolean = {
    if(debugging) {
      checkCoherenceWrtBellmanFord
    }
    consistent && contingentLinks.forall(l => isDelayPossible(l.src, l.dst, l.min.lb) && isConstraintPossible(l.src, l.dst, l.max.ub))
  }

  /** Removes all constraints that were recorded with this id */
  override def removeConstraintsWithID(id: ID): Boolean = ???

  override protected def addConstraint(u: TPRef, v: TPRef, w: Int): Unit =
    addMaxDelay(u, v, w)

  override protected def isConstraintPossible(u: TPRef, v: TPRef, w: Int): Boolean =
    w + rigidAwareDist(v, u) >= 0

  override def exportToDotFile(filename: String, printer: NodeEdgePrinter[Object, Object, LabeledEdge[Object, Object]]): Unit = ???

  /** Remove a timepoint and all associated constraints from the STN */
  override def removeTimePoint(tp: TPRef): Unit = ???

  /** Set the distance from the global start of the STN to tp to time */
  override def setTime(tp: TPRef, time: Int): Unit =
    optStart match {
      case Some(st) =>
        addMinDelay(st, tp, time)
        addMaxDelay(st, tp, time)
      case None => sys.error("This STN has no start timepoint")
    }


  /** Returns the minimal time from the start of the STN to u */
  override def getEarliestStartTime(u: TPRef): Int =
    optStart match {
      case Some(st) => minDelay(st, u)
      case None => sys.error("This STN has no start timepoint")
    }

  /** Returns the maximal time from the start of the STN to u */
  override def getLatestStartTime(u: TPRef): Int =
    optStart match {
      case Some(st) => maxDelay(st, u)
      case None => sys.error("This STN has no start timepoint")
    }


  override protected def addConstraintWithID(u: TPRef, v: TPRef, w: Int, id: ID): Unit =
    addConstraint(u, v, w)

  /**
    * Computes the max delay from a given timepoint to all others using Bellman-Ford on the original edges.
    * This is expensive (O(V*E)) but is useful for providing a reference to compare to when debugging.
    */
  private def distancesFromWithBellmanFord(from: TPRef) : Array[Int] = {
    // initialize distances
    val d = new Array[Int](99999)
    for(tp <- timepoints.asScala)
      d(tp.id) = INF
    d(from.id) = 0

    // compute distances
    val numIters = timepoints.size
    for(i <- 0 until numIters) {
      for(e <- originalEdges) {
        d(e.to.id) = Math.min(d(e.to.id), DistanceMatrix.plus(d(e.from.id), e.value))
      }
    }
    d
  }

  /**
    * Computes the max delay between two timepoints using Bellman-Ford on the original edges.
    * This is expensive (O(V*E)) but is useful for providing a reference to compare to when debugging.
    */
  private def distanceWithBellmanFord(from: TPRef, to: TPRef): Int = {
    distancesFromWithBellmanFord(from)(to.id)
  }

  /**
    * Determine whether the STN is consistent using Bellman-Ford on the original edges.
    * This is expensive (O(V*E)) but is useful for providing a reference to compare to when debugging.
    */
  private def consistencyWithBellmanFord(): Boolean = {
    // when possible, use "end" as the source as it normally linked with all other timepoints
    val from = optEnd match {
      case Some(end) => end
      case None => timepoints.head
    }
  val d = distancesFromWithBellmanFord(from)

    // if a distance can still be updated, there is a negative cycle
    for(e <- originalEdges) {
      if(d(e.to.id) > d(e.from.id) + e.value)
        return false
    }
    true
  }

  private def checkCoherenceWrtBellmanFord: Unit = {
    for(tp <- timepoints.asScala) {
      val d = distancesFromWithBellmanFord(tp)
      for(to <- timepoints.asScala) {
        assert(maxDelay(tp, to) == d(to.id))
      }
    }
  }

  override def enforceContingent(u: TPRef, v: TPRef, min: Int, max: Int, optID: Option[ID]): Unit = {
    addMinDelay(u, v, min)
    addMaxDelay(u, v, max)
    contingentLinks.append(new ContingentConstraint(u, v, IntExpression.lit(min), IntExpression.lit(max)))
  }

  override def getMaxDelay(u: TPRef, v: TPRef): Int = maxDelay(u, v)

  /** Returns a list of all constraints that were added to the STNU.
    * Each constraint is associated with flaw to distinguish between contingent and controllable ones. */
  override def constraints: IList[Constraint[ID]] = ???

  override def checksPseudoControllability: Boolean = true

  override def checksDynamicControllability: Boolean = false

  override def controllability: Controllability = Controllability.PSEUDO_CONTROLLABILITY

  /** If there is a contingent constraint [min, max] between those two timepoints, it returns
    * Some((min, max).
    * Otherwise, None is returned.
    */
  override def contingentDelay(from: TPRef, to: TPRef): Option[(Integer, Integer)] =
    contingentLinks.find(l => l.src == from && l.dst == to) match {
      case Some(x) => Some(x.min.lb.asInstanceOf[Integer], x.max.ub.asInstanceOf[Integer])
      case None => None
    }

  override def getMinDelay(u: TPRef, v: TPRef): Int = minDelay(u, v)

  override def addContingentTimePoint(tp: TPRef): Int = { assert(tp.genre.isContingent); recordTimePoint(tp) }

  override def addDispatchableTimePoint(tp: TPRef): Int = { assert(tp.genre.isDispatchable); recordTimePoint(tp) }

  override def start: Option[TPRef] = optStart

  override def end: Option[TPRef] = optEnd

  /** creates a virtual time point virt with the constraint virt -- [dist,dist] --> real */
  override def addVirtualTimePoint(virt: TPRef, real: TPRef, dist: Int): Unit = {
    recordTimePoint(virt)
    addMinDelay(virt, real, dist)
    addMaxDelay(virt, real, dist)
  }

  /** Set a constraint virt -- [dist,dist] --> real. virt must have been already recorded as a pending virtual TP */
  override def setVirtualTimePoint(virt: TPRef, real: TPRef, dist: Int): Unit = {
    addMinDelay(virt, real, dist)
    addMaxDelay(virt, real, dist)
  }

  /** Records a virtual time point that is still partially defined.
    * All constraints on this time point will only be processed when defined with method */
  override def addPendingVirtualTimePoint(virt: TPRef): Unit = {
    recordTimePoint(virt)
  }
}