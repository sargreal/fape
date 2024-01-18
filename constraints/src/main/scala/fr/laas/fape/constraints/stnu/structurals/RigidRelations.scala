package fr.laas.fape.constraints.stnu.structurals

import java.util

import fr.laas.fape.anml.model.concrete.TPRef
import java.util.{HashMap => JMap}

import scala.collection.JavaConverters._


private[structurals] final class AnchorOf(val anchor: TPRef, val distFromAnchor: Int)

final class RigidRelations(private val anchored: JMap[TPRef, JMap[TPRef,Int]],
                           private var _anchorOf: Array[AnchorOf],
                           private var _tpRefs: JMap[Int, TPRef]) {


  def this() = this(new JMap(), new Array[AnchorOf](10), new JMap())


  override def clone() : RigidRelations = {
    val newAnchored = new JMap[TPRef, JMap[TPRef,Int]]()
    for((tp,map) <- anchored.asScala)
      newAnchored.put(tp, new JMap[TPRef,Int](map))
    val newTpRefs = new JMap[Int, TPRef]()
    for((id,tp) <- _tpRefs.asScala)
      newTpRefs.put(id, tp)
    new RigidRelations(newAnchored, _anchorOf.clone(), newTpRefs)
  }

  def isAnchored(tp: TPRef) = _anchorOf.length > tp.id && _anchorOf(tp.id) != null
  def anchoredTimepoints: Iterator[TPRef] = {
    _anchorOf.indices.iterator.filter(_anchorOf(_) != null).map(_tpRefs.get(_)).filter(_ != null)
  }
  def isAnchor(tp: TPRef) = anchored.containsKey(tp)
  def anchorOf(tp: TPRef) = _anchorOf(tp.id).anchor
  def distFromAnchor(tp: TPRef) = _anchorOf(tp.id).distFromAnchor
  def distToAnchor(tp: TPRef) = -distFromAnchor(tp)
  private def addAnchored(tp: TPRef, anchorOf: AnchorOf): Unit = {
    if(_anchorOf.size <= tp.id) {
      _anchorOf = util.Arrays.copyOf(_anchorOf, math.max(tp.id+1 * 2, _anchorOf.size * 2))
    }
    _anchorOf(tp.id) = anchorOf
    _tpRefs.putIfAbsent(tp.id, tp)
  }
  def getTimepointsAnchoredTo(tp: TPRef) : List[TPRef] = anchored.get(tp).asScala.keys.toList

  def addAnchor(tp: TPRef): Unit = {
    anchored.put(tp, new JMap[TPRef, Int]())
    _tpRefs.putIfAbsent(tp.id, tp)
  }

  /** record a new rigid relation between those two timepoints.
    * At least one of them must be in the set already */
  def addRigidRelation(from: TPRef, to: TPRef, d: Int): Unit = {
    require(from != to)
    assert(isAnchor(from) && isAnchor(to))

    if(from.genre.isStructural && !to.genre.isStructural) {
      addRigidRelation(to, from, -d) // reverse to favor compiling structural timepoints
    } else {
      for (tp <- anchored.get(to).asScala.keys) {
        val distFromNewAnchor = d + distFromAnchor(tp)
        anchored.get(from).put(tp, distFromNewAnchor)
        addAnchored(tp, new AnchorOf(from, distFromNewAnchor))
      }
      anchored.remove(to)
      addAnchored(to, new AnchorOf(from, d))
      anchored.get(from).put(to, d)
    }
    _tpRefs.putIfAbsent(from.id, from)
    _tpRefs.putIfAbsent(to.id, to)
  }
}
