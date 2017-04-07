package fr.laas.fape.constraints.meta

import fr.laas.fape.constraints.bindings.InconsistentBindingConstraintNetwork
import fr.laas.fape.constraints.meta.constraints._
import fr.laas.fape.constraints.meta.domains.{BooleanDomain, Domain, EnumeratedDomain, IntervalDomain}
import fr.laas.fape.constraints.meta.events._
import fr.laas.fape.constraints.meta.logger.{ILogger, Logger}
import fr.laas.fape.constraints.meta.stn.core.StnWithStructurals
import fr.laas.fape.constraints.meta.stn.events.STNEventHandler
import fr.laas.fape.constraints.meta.stn.variables.{TemporalDelay, Timepoint}
import fr.laas.fape.constraints.meta.variables._

import scala.collection.mutable

class CSP(toClone: Option[CSP] = None) {
  implicit private val csp = this

  val domains : mutable.Map[VarWithDomain, Domain] = toClone match {
    case Some(base) => base.domains.clone()
    case None => mutable.Map()
  }

  val events: mutable.Queue[Event] = toClone match {
    case None => mutable.Queue()
    case Some(base) => base.events.clone()
  }

  val eventHandlers: mutable.ArrayBuffer[CSPEventHandler] = toClone match {
    case None => mutable.ArrayBuffer()
    case Some(base) => base.eventHandlers.map(handler => handler.clone(this))
  }

  val varStore: VariableStore = toClone match {
    case None => new VariableStore(this)
    case Some(base) => base.varStore.clone(this)
  }

  val constraints: ConstraintStore = toClone match {
    case Some(base) => base.constraints.clone(this)
    case None => new ConstraintStore(this)
  }

  val stn: StnWithStructurals = toClone match {
    case None => new StnWithStructurals()
    case Some(base) => base.stn.clone()
  }
  val stnBridge: STNEventHandler = toClone match {
    case None => new STNEventHandler()(this)
    case Some(base) => base.stnBridge.clone(this)
  }
  // set the STN listener to the one already in the event handlers to get notified of temporal variable updates
  stn.setDistanceChangeListener(stnBridge)

  val temporalOrigin = varStore.getTimepoint(":start:")
  val temporalHorizon = varStore.getTimepoint(":end:")

  override def clone : CSP = new CSP(Some(this))

  final val log : ILogger = new ILogger

  def dom(tp: Timepoint) : IntervalDomain =
    new IntervalDomain(stn.getEarliestTime(tp), stn.getLatestTime(tp))

  def dom(d: TemporalDelay) : IntervalDomain =
    new IntervalDomain(stn.getMinDelay(d.from, d.to), stn.getMaxDelay(d.from, d.to))

  def dom(v: IntVariable) : Domain =
    if(domains.contains(v))
      domains(v)
    else
      v.initialDomain

  def bind(variable: IntVariable, value: Int) {
    post(variable === value)
  }

  def updateDomain(variable: IntVariable, newDomain: Domain) {
    log.domainUpdate(variable, newDomain)
    if(newDomain.isEmpty) {
      throw new InconsistentBindingConstraintNetwork()
    } else if(variable.domain.size > newDomain.size) {
      events += DomainReduced(variable)
      domains(variable) = newDomain
    } else if(variable.domain.size < newDomain.size) {
      events += DomainExtended(variable)
      domains(variable) = newDomain
    }
  }

  def propagate(): Unit = {
    while(events.nonEmpty) {
      val e = events.dequeue()
      handleEvent(e)
    }
//    assert(sanityCheck())
  }

  def sanityCheck() : Boolean = {
    assert(events.isEmpty, "Can't sanity check: CSP has pending events")
    for(c <- constraints.active) {
      if(c.satisfaction != ConstraintSatisfaction.UNDEFINED) {
        println(report)
      }
      assert(c.satisfaction == ConstraintSatisfaction.UNDEFINED,
        s"Satisfaction of active constraint $c is not UNDEFINED but ${c.satisfaction}")
    }
    for(c <- constraints.satisfied)
      assert(c.isSatisfied, s"Constraint $c is not satisfied while in the satisfied list")
    true
  }

  def handleEvent(event: Event) {
    log.startEventHandling(event)
    constraints.handleEvent(event)
    event match {
      case NewConstraint(c) =>
        for(v <- c.variables) v match {
          case v: IntVariable if !domains.contains(v) => addVariable(v)
          case _ =>
        }
        c.onPost
        c.propagate(event)
        if(c.watched) {
          if(c.isSatisfied)
            addEvent(WatchedSatisfied(c))
          else if(c.isViolated)
            addEvent(WatchedViolated(c))
        }
      case e: DomainChange =>
        for(c <- constraints.activeWatching(e.variable)) {
          c.propagate(e)
        }
        for(c <- constraints.monitoredWatching(e.variable)) {
          if(c.isSatisfied)
            addEvent(WatchedSatisfied(c))
          else if(c.isViolated)
            addEvent(WatchedViolated(c))
        }
      case event: WatchedSatisfactionUpdate =>
        for(c <- constraints.monitoring(event.constraint)) {
          if(c.active) {
            c.propagate(event)
          }
          if(c.watched) {
            if(c.isSatisfied)
              addEvent(WatchedSatisfied(c))
            else if(c.isViolated)
              addEvent(WatchedViolated(c))
          }
        }
      case e: NewVariableEvent =>

      case Satisfied(c) =>
        if(c.watched)
          addEvent(WatchedSatisfied(c))
        // handled by constraint store
      case _: NewWatchedConstraint =>
    }
    stnBridge.handleEvent(event)
    for(h <- eventHandlers)
      h.handleEvent(event)
    log.endEventHandling(event)
  }

  def post(constraint: Constraint) {
    log.constraintPosted(constraint)
    addEvent(NewConstraint(constraint))
  }

  def postSubConstraint(constraint: Constraint, parent: Constraint) {
    post(constraint) // TODO, record relationship
  }

  def watchSubConstraint(subConstraint: Constraint, parent: Constraint) {
    constraints.addWatcher(subConstraint, parent)
  }

  def reified(constraint: Constraint) : ReificationVariable = {
    if(!varStore.hasVariableForRef(constraint)) {
      val variable = varStore.getReificationVariable(constraint)
      domains.put(variable, new BooleanDomain(Set(false, true)))
      post(new ReificationConstraint(variable, constraint))
    }
    varStore.getReificationVariable(constraint)
  }

  def setSatisfied(constraint: Constraint)  {
    assert(constraint.isSatisfied)
    addEvent(Satisfied(constraint))
  }

  def variable(ref: Any, dom: Set[Int]) : IntVariable = {
    val v = new IntVariable(new EnumeratedDomain(dom), Some(ref))
    addVariable(v)
    v
  }

  def variable(ref: Any, lb: Int, ub: Int) : IntVariable = {
    val v = new IntVariable(new IntervalDomain(lb, ub), Some(ref))
    addVariable(v)
    v
  }

  def addVariable(variable: IntVariable)  {
    assert(!hasVariable(variable))
    domains.put(variable, variable.initialDomain)
    if(variable.ref.nonEmpty)
      varStore.setVariableForRef(variable.ref.get, variable)
    variableAdded(variable)
  }

  /** Records an event notifying of the variable addition + some sanity checks */
  def variableAdded(variable: IVar) {
    variable match {
      case v: IntVariable => assert(domains.contains(v), "Variable has no domain")
      case _ =>
    }
    addEvent(NewVariableEvent(variable))
  }

  def addEvent(event: Event) {
    events += event
  }

  def hasVariable(variable: IntVariable) : Boolean = domains.contains(variable)

  def nextVarId() = varStore.getNextVariableId()


  def report : String = {
    val str = new StringBuilder
    val vars = constraints.all.flatMap(c => c.variables(csp)).collect{ case v: VarWithDomain => v }
    for(v <- vars) {
      str.append(s"$v = ${v.domain}\n")
    }
    str.append("%% ACTIVE CONSTRAINTS\n")
    for(c <- constraints.active.toSeq.sortBy(_.toString))
      str.append(s"$c  ${c.satisfaction}\n")

    str.append("%% SATISFIED CONSTRAINTS\n")
    for(c <- constraints.satisfied.toSeq.sortBy(_.toString))
      str.append(s"$c  ${c.satisfaction}\n")
    str.toString
  }

  def makespan: Int = dom(temporalHorizon).lb

  def isSolution : Boolean = {
    assert(events.isEmpty, "There are pending events in this CSP, can't check if it is a solution")
    assert(sanityCheck())
    constraints.active.isEmpty
  }
}