package fr.laas.fape.graph.core.impl

import fr.laas.fape.graph.core.{DirectedGraph, Edge}

import scala.collection.mutable

abstract class DirectedAdjacencyList[V, EL, E <: Edge[V]](val mOutEdges : mutable.ArrayBuffer[List[E]],
                                    val mInEdges : mutable.ArrayBuffer[List[E]],
                                    val mIndexes : mutable.Map[V, Int],
                                    val mVertices : mutable.ArrayBuffer[V])
  extends DirectedGraph[V,EL,E] {

  var mNumVertices = mIndexes.size

  def this() = this(new mutable.ArrayBuffer[List[E]](0), new mutable.ArrayBuffer[List[E]](0),
                    mutable.Map[V, Int](), new mutable.ArrayBuffer[V](0))



  def addVertex(v:V) : Int = {
    assert(!contains(v), "Graph already contains vertex: "+v)

    val vertId = this.numVertices
    mInEdges.append(List[E]())
    mOutEdges.append(List[E]())
    mVertices.append(v)
    mIndexes.+=((v, vertId))

    mNumVertices += 1
    return vertId
  }

  def vertices = mVertices.filter(v => contains(v)).toSeq

  def addEdge(e:E) { ??? }

  def addEdgeImpl(e:E) {
    val uId = mIndexes(e.u)
    val vId = mIndexes(e.v)

    mOutEdges(uId) = e :: mOutEdges(uId)
    mInEdges(vId) = e :: mInEdges(vId)
  }

  def outEdges(u:V) : Seq[E] = mOutEdges(mIndexes(u))

  def inEdges(v:V) : Seq[E] = mInEdges(mIndexes(v))

  def deleteEdges(u:V, v:V) {
    val uId = mIndexes(u)
    val vId = mIndexes(v)
    mOutEdges(uId) = mOutEdges(uId).filter(edge => edge.v != v)
    mInEdges(vId) = mInEdges(vId).filter(edge => edge.u != u)
  }

  def deleteEdge(e:E): Unit = {
    val uId = mIndexes(e.u)
    val vId = mIndexes(e.v)
    mOutEdges(uId) = mOutEdges(uId).filter(edge => !(edge eq e))
    mInEdges(vId) = mInEdges(vId).filter(edge => !(edge eq e))
  }

  def edges : Seq[E] = {
    var alledges = List[E]()
    mOutEdges.foreach(edgelist => alledges = alledges ++ edgelist)
    alledges
  }

  def contains(v: V): Boolean = mIndexes.contains(v)

  def numVertices = mNumVertices

  def deleteVertex(v: V) {
    val id = mIndexes(v)
    mOutEdges(id) = List[E]()
    mInEdges(id) = List[E]()
    // mVertices(id) = null
    mIndexes.remove(v)
    mNumVertices -= 1
  }
}
