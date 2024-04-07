package fr.laas.fape.constraints.bindings

import java.util

import fr.laas.fape.anml.model.concrete.VarRef

import scala.collection.mutable
import scala.collection.JavaConverters._

trait Constraint {
  def vars: Seq[VarRef]
  def involves(v:VarRef)  : Boolean
  def propagate(csp: BindingConstraintNetwork)
}

class NAryConstraint(val vars:Seq[VarRef], val allowedTuple: ExtensionConstraint) extends Constraint {
  require(vars.size == allowedTuple.numVars())
  val varSet : collection.Set[Int] = mutable.Set[Int](vars.map(_.id): _*)
  override def involves(v: VarRef) = varSet.contains(v.id)

  override def propagate(csp: BindingConstraintNetwork) {
    // println(toString())

    val domains = vars.map(v => csp.rawDomain(v).vals).toArray
    val restrictedDomains = allowedTuple.restrictedDomains(domains)

    for(i <- domains.indices) {
      if(domains(i).size > restrictedDomains(i).size) {
        csp.restrictDomain(vars(i), new Domain(restrictedDomains(i)))
      }
    }
  }

  override def toString: String = s"NAryConstraint($vars, $allowedTuple)"
}

class InSetConstraint(val left:VarRef, val right:Set[VarRef]) extends Constraint {
  def this(left: VarRef, right: util.Collection[VarRef]) = this(left, right.asScala.toSet)
  val vars: Seq[VarRef] = left :: right.toList
  override def propagate(csp: BindingConstraintNetwork): Unit = {
    val domains = right.map(v => csp.rawDomain(v))
    val union = domains.tail.foldLeft(domains.head)((acc, dom) => acc.union(dom))
    csp.restrictDomain(left, union, Some(this))
  }

  override def involves(v: VarRef): Boolean = right.contains(v)
}


