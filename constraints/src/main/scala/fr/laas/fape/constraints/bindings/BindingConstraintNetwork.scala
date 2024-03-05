package fr.laas.fape.constraints.bindings

import java.util

import fr.laas.fape.anml.model.Type
import fr.laas.fape.anml.model.concrete.VarRef
import fr.laas.fape.constraints.bindings.BindingConstraintNetwork.ExtID

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object BindingConstraintNetwork {
  type DomID = Int
  type ExtID = Int

  var cnt = 0
}

class BindingConstraintNetwork(toCopy: Option[BindingConstraintNetwork]) {

  def this() = this(None)

  import BindingConstraintNetwork.DomID

  private val increment = 10

  var domIds : Array[DomID] = null
  var variables : Array[VarRef] = null 
  var domains : Array[Domain] = null

  var vars : ArrayBuffer[ArrayBuffer[VarRef]] = null
  var different : Array[util.BitSet] = null
  var values : ArrayBuffer[String] = null
  var valuesIds : Map[String, Int] = null

  var defaultIntDomain : ArrayBuffer[Domain] = null

  /** Extension constraints */
  var extensionConstraints : Map[String, ExtensionConstraint] = null

  var constraints : mutable.Buffer[Constraint] = null
  var watchers : mutable.Map[VarRef, ArrayBuffer[Constraint]] = null

  var pendingConstraints : mutable.Set[Constraint] = null

  var unusedDomainIds : mutable.Set[DomID] = null

  var hasEmptyDomains = false

  var listener : IntBindingListener[VarRef] = null

  toCopy match {
    case Some(o) =>
      domIds = o.domIds.clone()
      domains = o.domains.clone()
      variables = o.variables.clone()
      vars = o.vars.map(x => x.clone())
      different = o.different.map(x => x.clone().asInstanceOf[util.BitSet])
      values = o.values
      valuesIds = o.valuesIds
      defaultIntDomain = o.defaultIntDomain
      extensionConstraints = o.extensionConstraints
      constraints = o.constraints.clone()
      watchers = mutable.Map(o.watchers.mapValues(_.clone()).toSeq: _*)
      unusedDomainIds = o.unusedDomainIds.clone()
      pendingConstraints = o.pendingConstraints.clone()
    case None =>
      BindingConstraintNetwork.cnt += 1
      domIds = Array.fill(10)(-1) //mutable.Map[VarRef, DomID]()
      variables = Array.fill(10)(null)
      domains = Array.fill(10)(null) //mutable.Map[DomID, ValuesHolder]()
      vars = ArrayBuffer[ArrayBuffer[VarRef]]()
      different = Array.fill(increment)(new util.BitSet())

      values = ArrayBuffer[String]()
      valuesIds = Map[String, Int]()
      defaultIntDomain = ArrayBuffer(new Domain(Set()))

      extensionConstraints = Map()
      constraints = ArrayBuffer()
      watchers = mutable.Map()

      unusedDomainIds = mutable.Set()
      pendingConstraints = mutable.Set()
  }

  protected[bindings] def allVars = variables

  def domID(v: VarRef) : DomID = domIds(v.id)
  def rawDomainByID(id: DomID) = domains(id)

  private def allDomIds = vars.indices.filterNot(unusedDomainIds.contains)

  private def newDomID() : DomID = {
    val id =
      if(unusedDomainIds.nonEmpty) {
        val next = unusedDomainIds.head
        unusedDomainIds -= next
        next
      } else {
        val next = vars.size
        next
      }

    if(vars.size > id) vars(id) = ArrayBuffer[VarRef]()
    else vars += ArrayBuffer[VarRef]()

    if(id == different.length) {
      // replace with bigger
      val prevSize = different.length
      different = util.Arrays.copyOf(different, different.length*2)
      assert(different(prevSize) == null && different(prevSize-1) != null)
      for(i <- prevSize until different.length)
        different(i) = new util.BitSet()
    }

    id
  }

  def rawDomain(v: VarRef) : Domain = domains(domID(v))

  def isDiff(v1: VarRef, v2: VarRef): Boolean = {
    assert(different(domID(v1)).get(domID(v2)) == different(domID(v2)).get(domID(v1)))
    different(domID(v1)).get(domID(v2))
  }

  private def domainChanged(id: DomID, causedByExtended: Option[Constraint]): Unit = {
    if(domains(id).size == 0) {
      hasEmptyDomains = true
      throw new VarWithEmptyDomain(vars(id).toList.asJava)
    }

    if(domains(id).size == 1) {
      // check difference constraints
      val uniqueValue = domains(id).min
      val diff = different(id)
      var o = vars.size -1
      while(o >= 0) {
        if(diff.get(o)) {
          if(!unusedDomainIds.contains(o)) {
            if(domains(o).contains(uniqueValue)) {
              domains(o) = domains(o).remove(uniqueValue)
              domainChanged(o, None)
            }
          }
        }
        o -= 1
      }
    }

    for(v <- vars(id)) {
      if(watchers.contains(v))
        pendingConstraints ++= watchers(v)
    }

    // if it is a integer varaible that got binded, notify the listener if any
    if(listener != null && domains(id).size == 1 && isIntegerVar(vars(id).head)) {
      val value = domains(id).min
      for(v <- vars(id)) {
        assert(isIntegerVar(v))
        listener.onBinded(v, value)
      }
    }
  }


  def setListener(listener: IntBindingListener[VarRef]) { this.listener = listener }

  def domainOfIntVar(v: VarRef): util.List[Integer] = {
    assert(isIntegerVar(v))
    rawDomain(v).vals.map(_.asInstanceOf[Integer]).toList.asJava
  }

  def isIntegerVar(v: VarRef): Boolean = v.getType.isNumeric

  def stringValuesAsDomain(stringDomain: util.Collection[String]): Domain =
    new Domain(stringDomain.asScala.map(valuesIds(_)))
  
  def getVarFromStringValue(value: String): VarRef = {
    val id = valuesIds(value)
    val domID = domIds(id)
    vars(domID).head
  }

  def typeOf(v: VarRef): Type = v.typ

  def unifiable(a: VarRef, b: VarRef): Boolean =
    domID(a) == domID(b) || (!isDiff(a, b) && rawDomain(a).hasOneCommonElement(rawDomain(b)))

  def domainOf(v: VarRef): util.List[String] =
    rawDomain(v).values().asScala.map(values(_)).toList.asJava

  def unified(a: VarRef, b: VarRef): Boolean =
    domID(a) == domID(b) || domainSize(a) == 1 && domainSize(b) == 1 && rawDomain(a).min == rawDomain(b).min

  def recordEmptyNAryConstraint(setID: String, isLastValInteger: Boolean, numVariables: Int) = {
    assert(!extensionConstraints.contains(setID))
    extensionConstraints += ((setID, new ExtensionConstraint(setID, isLastValInteger, numVariables)))
  }

  def addAllowedTupleToNAryConstraint(setID: String, values: util.List[String]): Unit = {
    if(!extensionConstraints.contains(setID)) { //TODO: should force usage of record
      extensionConstraints += ((setID, new ExtensionConstraint(setID, false, values.size())))
    }
    val valuesAsIDs = values.asScala.map(valuesIds(_).asInstanceOf[Integer]).toList.asJava
    extensionConstraints(setID).addValues(valuesAsIDs)
  }

  def addAllowedTupleToNAryConstraint(setID: String, values: util.List[String], lastVal: Int): Unit = {
    if(!extensionConstraints.contains(setID)) {
      extensionConstraints += ((setID, new ExtensionConstraint(setID, true, values.size()+1)))
    }
    val valuesAsIDs = values.asScala
      .map(valuesIds(_).asInstanceOf[Integer])

    addPossibleValue(lastVal)
    valuesAsIDs.append(lastVal)

    extensionConstraints(setID).addValues(valuesAsIDs.asJava)
  }


  def separated(a: VarRef, b: VarRef): Boolean =
    isDiff(a, b)

  def separable(a: VarRef, b: VarRef): Boolean =
    !isDiff(a, b) && !unified(a, b)

  def addPossibleValue(value: String) {
    assert(!valuesIds.contains(value))
    valuesIds += ((value, values.size))
    values += value
  }

  def addPossibleValue(value: Int): Unit = {
    if(!defaultIntDomain(0).contains(value)) {
      defaultIntDomain(0) = defaultIntDomain.head.add(value)
    }
  }

  def addNAryConstraint(variables: util.List[VarRef], setID: String): Unit = {
    assert(variables.asScala.forall(v => domID(v) >= 0))
    assert(extensionConstraints.contains(setID),
      s"No recorded allowed values with name \'$setID\'. This usually means that no value was assigned to it.")
    val c = new NAryConstraint(variables.asScala, extensionConstraints(setID))
    addConstraint(c)
  }

  def addConstraint(c: Constraint): Unit = {
    constraints += c
    for(v <- c.vars) {
      if(!watchers.contains(v))
        watchers(v) = new ArrayBuffer[Constraint]()
      watchers(v) += c
    }
    pendingConstraints += c
  }

  def restrictIntDomain(v: VarRef, toValues: util.Collection[Integer]): Unit =
    restrictDomain(v, intValuesAsDomain(toValues))

  def addSeparationConstraint(a: VarRef, b: VarRef): Unit = {
    if(domID(a) == domID(b)) {
      hasEmptyDomains = true
      throw new VarWithEmptyDomain(List(a, b).asJava)
    }

    different(domID(a)).set(domID(b))
    different(domID(b)).set(domID(a))

    if(domainSize(a) == 1)
      domainChanged(domID(a), None)
    if(domainSize(b) == 1)
      domainChanged(domID(b), None)
  }

  def isConsistent: Boolean = {
    while(pendingConstraints.nonEmpty && !hasEmptyDomains) {
      val cur = pendingConstraints.head
      pendingConstraints -= cur

      checkConstraint(cur)
    }

    !hasEmptyDomains
  }

  def checkConstraint(constraint: Constraint): Unit = {
    constraint.propagate(this)
  }

  def domainAsString(v: VarRef): String =
    if(isIntegerVar(v))
      "{" + rawDomain(v).vals.mkString(", ") + "}"
    else
      "{"+ rawDomain(v).vals.map(values(_)).mkString(", ") +"}"

  def report(): String =
    allDomIds.map(id => (id, "["+ vars(id).mkString(", ") +"]", "  "+domainAsString(vars(id).head))).mkString("\n")

  private def merge(id1: DomID, id2: DomID) : Unit = {
    val newDom = domains(id1).intersect(domains(id2))
    val domainUpdated = newDom.size < domains(id1).size || newDom.size < domains(id2).size

    domains(id1) = newDom
    vars(id1) = vars(id1) ++ vars(id2)
    for(i <- 0 until different(id1).size) {
      if(different(id2).get(i)) {
        different(id1).set(i)
        different(i).set(id1)
      }
    }
    for(v <- vars(id2))
      domIds(v.id) = id1

    unusedDomainIds += id2
    vars(id2) = new ArrayBuffer[VarRef]()
    for(i <- different.indices) {
      different(i).clear(id2)
      different(id2).clear(i)
    }
    domains(id2) = null // -= id2

    // make sure new constraints are propagated
    domainChanged(id1, None)

    assert(allDomIds.forall(id => vars(id).nonEmpty))
  }

  def AddUnificationConstraint(a: VarRef, b: VarRef): Unit = {
    if(domID(a) != domID(b)) {
      merge(domID(a), domID(b))
    }
  }

  def restrictDomain(v: VarRef, domain: Domain): Boolean = restrictDomain(v, domain, None)

  def restrictDomain(v: VarRef, domain: Domain, origin: Option[Constraint]): Boolean = {
    val newDom = rawDomain(v).intersect(domain)
    val modified = newDom.size < rawDomain(v).size
    if(modified) {
      domains(domID(v)) = newDom
      domainChanged(domID(v), origin)
    }
    modified
  }

  def restrictDomain(v: VarRef, toValues: util.Collection[String]): Unit =
    restrictDomain(v, stringValuesAsDomain(toValues))

  def keepValuesAboveOrEqualTo(v: VarRef, min: Int): Unit = {
    assert(isIntegerVar(v))

    val initialDomain = domainOfIntVar(v).asScala
    val newDomain = initialDomain.filter(i => i >= min)
    if(newDomain.size < initialDomain.size) {
      domains(domID(v)) = intValuesAsDomain(newDomain.asJava)
      domainChanged(domID(v), None)
    }
  }
  def keepValuesBelowOrEqualTo(v: VarRef, max: Int): Unit = {
    assert(isIntegerVar(v))

    val initialDomain = domainOfIntVar(v).asScala
    val newDomain = initialDomain.filter(i => i <= max)
    if(newDomain.size < initialDomain.size) {
      domains(domID(v)) = intValuesAsDomain(newDomain.asJava)
      domainChanged(domID(v), None)
    }
  }

  def getUnboundVariables: util.List[VarRef] = {
    val unboundDomains =
      for (domId <- domains.indices ; if domains(domId) != null && domains(domId).size != 1)
        yield domId
    unboundDomains.map(vars(_).head).filter(!isIntegerVar(_)).toList.asJava
  }

  private val typeDomainCache = mutable.Map[Type,Domain]()
  def defaultDomain(t: Type) : Domain = {
    if(!typeDomainCache.contains(t)) {
      val dom = stringValuesAsDomain(t.instances.map(i => i.instance).asJava)
      typeDomainCache(t) = dom
      dom
    } else {
      typeDomainCache(t)
    }
  }

  def addVariable(v: VarRef): Unit = {
    addVariable(v, defaultDomain(v.getType))
  }

  def addVariable(v: VarRef, domain: util.Collection[String]): Unit = {
    assert(!v.typ.isNumeric)
    addVariable(v, stringValuesAsDomain(domain))
  }

  private def ensureSpaceForVar(v:VarRef, futureDomID: Int): Unit = {
    if(v.id >= domIds.length) {
      val tmp = domIds
      domIds = Array.fill(Math.max(domIds.length*2, v.id+1))(-1)
      Array.copy(tmp, 0, domIds, 0, tmp.length)
      variables = util.Arrays.copyOf(variables, Math.max(variables.length*2, v.id+1))
    }
    if(futureDomID >= domains.length) {
      domains = util.Arrays.copyOf(domains, Math.max(domains.length*2, futureDomID+1))
    }

    assert(domIds(v.id) == -1)
    assert(variables(v.id) == null)
    assert(domains(futureDomID) == null)
  }

  private def addVariable(v:VarRef, dom: Domain) {
    assert(!contains(v))
    val domID = newDomID()
    ensureSpaceForVar(v, domID)

    domIds(v.id) = domID
    variables(v.id) = v
    vars(domID) += v
    domains(domID) = dom
    domainChanged(domID, None)

    assert(vars(domID).size == 1)
  }

  def isRecorded(v: VarRef) : Boolean = domIds.length > v.id && domIds(v.id) != -1

  def contains(v: VarRef): Boolean = isRecorded(v)

  def DeepCopy(): BindingConstraintNetwork =
    new BindingConstraintNetwork(Some(this))

  def domainSize(v: VarRef): Integer = rawDomain(v).size

  def addIntVariable(v: VarRef): Unit = {
    assert(v.typ.isNumeric)
    addVariable(v, defaultIntDomain.head)
  }

  def addIntVariable(v: VarRef, dom: Domain) : Unit = {
    assert(v.typ.isNumeric)
    addVariable(v, dom)
  }

  def addIntVariable(v: VarRef, domain: util.Collection[Integer]): Unit = {
    assert(v.typ.isNumeric)
    addVariable(v, intValuesAsDomain(domain))
  }

  @Deprecated
  def intValuesAsDomain(intDomain: util.Collection[Integer]): Domain = {
    val valuesIds =
      for(value <- intDomain.asScala) yield {
        if(!defaultIntDomain.head.contains(value))
          addPossibleValue(value)
        value.toInt
      }
    new Domain(valuesIds)
  }

  @Deprecated
  def intValueOfRawID(valueID: Integer): Integer = valueID
}
