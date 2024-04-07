package fr.laas.fape.graph.core.impl.intindexed

import fr.laas.fape.graph.core.{DirectedGraph, Edge}

import scala.collection.mutable

abstract class DirectedIIAdjList[EL, E <: Edge[Int]](val mOutEdges : mutable.ArrayBuffer[List[E]],
                                                  val mInEdges : mutable.ArrayBuffer[List[E]])
  extends DirectedGraph[Int,EL,E] {

  var mNumVertices = mOutEdges.length




  def addVertex() : Int = {
    val id = mNumVertices
    mInEdges.append(List[E]())
    mOutEdges.append(List[E]())
    mNumVertices += 1
    id
  }

  /**
   * Warning, this method is just a wrapper around addVertex(). It is added for compatibility with other graphs.
   * Any call on this method with v != numVertices will fail on assertion.
   *
   * Proper call is g.addVertex(g.numVertices)
   * If possible, prefer using addVertex() which return the id of the inserted vertex.
   * @param v
   * @return
   */
  def addVertex(v:Int) : Int = {
    assert(numVertices == v, "Vertex ids have to be the strictly growing (%s != %s)".format(numVertices, v))
    addVertex()
  }

  def vertices = 0 to numVertices-1

  def addEdge(e:E) { ??? }

  def addEdgeImpl(e:E) {
    mOutEdges(e.u) = e :: mOutEdges(e.u)
    mInEdges(e.v) = e :: mInEdges(e.v)
  }

  def outEdges(u:Int) : Seq[E] = mOutEdges(u)

  def inEdges(v:Int) : Seq[E] = mInEdges(v)

  def deleteEdges(u:Int, v:Int) {
    mOutEdges(u) = mOutEdges(u).filter(edge => edge.v != v)
    mInEdges(v) = mInEdges(v).filter(edge => edge.u != u)
  }

  def deleteEdge(e:E) {
    mOutEdges(e.u) = mOutEdges(e.u).filter(edge => !(edge eq e))
    mInEdges(e.v) = mInEdges(e.v).filter(edge => !(edge eq e))
  }

  def edges() : Seq[E] = {
    var alledges = List[E]()
    mOutEdges.foreach(edgelist => alledges = alledges ++ edgelist)
    alledges
  }

  def contains(v:Int): Boolean = 0 <= v && v < numVertices

  def numVertices = mNumVertices

  def deleteVertex(v:Int) {
    throw new UnsupportedOperationException("Cannot remove a vertex by reference in an adjacency list " +
      "since the size of the adjacency list cannot be reduced without losing vertexes.")
  }
}