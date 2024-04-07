package fr.laas.fape.planning.core.planning.states;

import fr.laas.fape.constraints.bindings.Domain;
import fr.laas.fape.gui.ChartLine;
import fr.laas.fape.gui.RectElem;
import fr.laas.fape.gui.TextLabel;
import fr.laas.fape.gui.TimedCanvas;
import fr.laas.fape.planning.core.planning.grounding.DisjunctiveFluent;
import fr.laas.fape.planning.core.planning.grounding.Fluent;
import fr.laas.fape.planning.core.planning.grounding.GAction;
import fr.laas.fape.planning.core.planning.grounding.TempFluents;
import fr.laas.fape.planning.core.planning.planner.Counters;
import fr.laas.fape.planning.core.planning.planner.Planner;
import fr.laas.fape.planning.core.planning.planner.PlanningOptions;
import fr.laas.fape.anml.model.concrete.*;
import fr.laas.fape.planning.core.planning.search.Handler;
import fr.laas.fape.planning.core.planning.search.flaws.finders.FlawFinder;
import fr.laas.fape.planning.core.planning.search.flaws.flaws.Flaw;
import fr.laas.fape.planning.core.planning.search.flaws.resolvers.Resolver;
import fr.laas.fape.planning.core.planning.states.modification.ChronicleInsertion;
import fr.laas.fape.planning.core.planning.states.modification.PartialPlanModification;
import fr.laas.fape.planning.core.planning.tasknetworks.TaskNetworkManager;
import fr.laas.fape.planning.core.planning.tasknetworks.TNNode;
import fr.laas.fape.planning.core.planning.timelines.ChainComponent;
import fr.laas.fape.planning.core.planning.timelines.Timeline;
import fr.laas.fape.planning.core.planning.timelines.TimelinesManager;
import fr.laas.fape.planning.exceptions.FAPEException;
import fr.laas.fape.planning.util.EffSet;
import fr.laas.fape.planning.util.Pair;
import fr.laas.fape.planning.util.Reporter;
import fr.laas.fape.planning.util.TinyLogger;
import fr.laas.fape.structures.IRSet;
import lombok.Getter;
import lombok.Setter;
import fr.laas.fape.anml.model.AnmlProblem;
import fr.laas.fape.anml.model.LStatementRef;
import fr.laas.fape.anml.model.ParameterizedStateVariable;
import fr.laas.fape.anml.model.abs.AbstractAction;
import fr.laas.fape.anml.model.concrete.statements.*;
import fr.laas.fape.anml.pending.IntExpression;
import fr.laas.fape.anml.pending.LStateVariable;
import fr.laas.fape.anml.pending.StateVariable;
import fr.laas.fape.constraints.MetaCSP;
import fr.laas.fape.constraints.bindings.InSetConstraint;
import fr.laas.fape.constraints.stnu.Controllability;
import fr.laas.fape.constraints.stnu.InconsistentTemporalNetwork;
import scala.Option;
import scala.Tuple2;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class PartialPlan implements Reporter {

    static int idCounter = 0;
    public final int mID;
    int depth;

    @Setter @Getter
    /** Number of the last decomposition that was used in this state.
     * This is used to always consider the method in the order they are given in the domain file.*/
    int lastDecompositionNumber = 0;

    /**
     *
     */
    public final TimelinesManager tdb;
    public final MetaCSP csp;

    public final TaskNetworkManager taskNet;

    public final RefCounter refCounter;

    /** Maps a constant parameterized variable to a variable in the CSP */
    private final Map<ParameterizedStateVariable, VarRef> stateVarsToVariables;

    private boolean isDeadEnd = false;
    private boolean isConsistent = true;

    /**
     * Keep tracks of statements that must be supported by a particular
     * decomposition. (e.g. by a statements which is a consequence of that
     * decomposition). This map is populated when a decomposition is chosen as a
     * resolver for an unsupported database.
     */
    private LinkedList<Pair<Integer, Action>> supportConstraints;

    public final AnmlProblem pb;

    /** Current planner instance handling this state */
    public Planner pl;

    public final Controllability controllability;

    /**
     *  The set of actions that might be addable in this state.
     * Those actions must (i) be reachable (plannning graph)
     * (ii) be derivable from an reachable action if motivated.
     * This field is filled by PlanningGraphReachibility when needed.
     */
    public EffSet<GAction> addableActions;
    public Set<AbstractAction> addableTemplates;

    /**
     * Contains all ground versions of fluents in the state with their associated time points
     */
    public List<TempFluents> fluents = null;
    public List<TempFluents> fluentsWithChange = null;

    /** Extensions of a state that will be inherited by its children */
    private final List<StateExtension> extensions;


    /** All modifications (i.e. resolvers) that have been applied to build this state */
    private final List<PartialPlanModification> modifications;
    public List<PartialPlanModification> getStateModifications() { return modifications; }

    /**
     * Index of the latest applied StateModifier in pb.jModifiers()
     */
    private int problemRevision;

    private int earliestExecution = 0;
    public int getEarliestExecution() { return earliestExecution; }
    public void setEarliestExecution(int earliestExecution) { this.earliestExecution = earliestExecution; }


    /**
     * this constructor is only for the initial state!! other states are
     * constructed from from the existing states
     */
    public PartialPlan(AnmlProblem pb, Controllability controllability) {
        this.mID = idCounter++;
        this.pb = pb;
        this.depth = 0;
        this.controllability = controllability;
        this.refCounter = pb.refCounter().clone();
        this.modifications = new ArrayList<>();
        tdb = new TimelinesManager(this);
        csp = fr.laas.fape.constraints.Factory.getMetaWithGivenControllability(controllability);
        taskNet = new TaskNetworkManager();

        addableActions = null;
        addableTemplates = null;
        stateVarsToVariables = new HashMap<>();

        supportConstraints = new LinkedList<>();
        extensions = new ArrayList<>();
        extensions.add(new HierarchicalConstraints(this));
        extensions.add(new OpenGoalSupportersCache(this));
        extensions.add(new CausalNetworkExt(this));
        extensions.add(new ThreatsCache(this));

        // Insert all problem-defined modifications into the state
        problemRevision = -1;
        update();
    }

    /**
     * Produces a new State with the same content as state in parameter.
     *
     * @param st State to copy
     */
    public PartialPlan(PartialPlan st, int id) {
        this(st, id, true);
    }

    /**
     * Produces a new State with the same content as state in parameter.
     *
     * @param st State to copy
     */
    public PartialPlan(PartialPlan st, int id, boolean keepPlanner) {
        this.mID = id;
        this.depth = st.depth +1;
        pb = st.pb;
        if (keepPlanner)
        pl = st.pl;
        this.controllability = st.controllability;
        this.modifications = new ArrayList<>(st.modifications);
        this.refCounter = st.refCounter.clone();
        this.earliestExecution = st.earliestExecution;
        isDeadEnd = st.isDeadEnd;
        problemRevision = st.problemRevision;
        csp = new MetaCSP(st.csp);
        tdb = new TimelinesManager(st.tdb, this); //st.tdb.deepCopy();
        taskNet = st.taskNet.deepCopy();
        supportConstraints = new LinkedList<>(st.supportConstraints);
        stateVarsToVariables = new HashMap<>(st.stateVarsToVariables);
        addableActions = st.addableActions != null ? st.addableActions.clone() : null;
        addableTemplates = st.addableTemplates != null ? new HashSet<>(st.addableTemplates) : null;

        extensions = st.extensions.stream().map(ext -> ext.clone(this)).collect(Collectors.toList());
    }

    /** Returns the depth of this node in the search space */
    public int getDepth() { return depth; }

    public void setPlanner(Planner planner) {
        assert pl == null : "This state is already attached to a planner.";
        this.pl = planner;
        for(Handler h : getHandlers())
            h.stateBindedToPlanner(this, planner);
    }

    public void setDeadEnd() { isDeadEnd = true; }

    public PartialPlan cc(boolean keepPlanner) {
        return cc(PartialPlan.idCounter++, keepPlanner);
    }

    public PartialPlan cc(int newID) {
        return cc(newID, true);
    }

    public PartialPlan cc(int newID, boolean keepPlanner) {
        Counters.inc("plan-copy");
        return new PartialPlan(this, newID, keepPlanner);
    }

    // public PartialPlan plainClone() {
    //     PartialPlan newPlan = new PartialPlan(this.pb, this.controllability);
    //     // Rebuild the plan from scratch using only the actions and tasks
        
    //     // First value is the action/task to insert, second value is the parentin the new plan
    //     ArrayDeque<Pair<Action,Task>> actions = new ArrayDeque<>();
    //     ArrayDeque<Pair<Task,Action>> tasks = new ArrayDeque<>();

    //     for (TNNode n : this.taskNet.roots()) {
    //         if (n.isAction()) {
    //             actions.add(new Pair<>(n.asAction(), null));
    //         } else {
    //             tasks.add(new Pair<>(n.asTask(), null));
    //         }
    //     }

    //     // Add the tasks and actions following the hierarchy
    //     while(!actions.isEmpty() || !tasks.isEmpty()) {
    //         if(!actions.isEmpty()) {
                
    //             Pair<Action, Task> pairToInsert = actions.poll();
    //             Action original = pairToInsert.value1;
    //             Task parent = pairToInsert.value2;
    //             Action newAction;
    //             if (parent == null) {
    //                 newAction = Factory.getStandaloneAction(pb, original.abs(), refCounter);
    //             } else {
    //                 newAction = Factory.getSupportingAction(pb, original.abs(), parent, refCounter);
    //             }
    //             // If the action is not pending, all bindings and timepoints have to be fixed
    //             switch (original.status()) {
    //                 case EXECUTED:
    //                     newPlan.csp.stn().setTime(newAction.start(), getEarliestStartTime(original.start()));
    //                 case EXECUTING:
    //                     newPlan.csp.stn().setTime(newAction.end(), getEarliestStartTime(original.end()));
    //                     // Restrict the domain of the variables to the values they have in the original plan
    //                     // Complicated here is that the variables are not the same, so we have to find the corresponding variable
    //                     // using the instance names
    //                     for (VarRef var : newAction.vars()) {
    //                         if (var instanceof InstanceRef) {
    //                             String instanceName = ((InstanceRef) var).instance();
    //                             VarRef originalVar = original.vars().stream()
    //                                     .filter(v -> v instanceof InstanceRef)
    //                                     .filter(v -> ((InstanceRef) v).instance().equals(instanceName))
    //                                     .findFirst().orElse(null);
    //                             if (originalVar != null) {
    //                                 Domain originalDomain = original.chronicle().bindings().domainOf(originalVar);
    //                                 newPlan.csp.bindings().restrictDomain(var, originalDomain);
    //                             }
    //                         }
    //                     }
    //                     // for (BindingConstraint bc : original.bindingConstraints()) {
    //                     //     BindingConstraint newBc;
    //                     //     TinyLogger.LogInfo(bc.toString());
    //                     //     if (bc instanceof AssignmentConstraint) {
    //                     //         newBc = new AssignmentConstraint(((AssignmentConstraint) bc).sv(), ((AssignmentConstraint) bc).variable());
    //                     //     } else if (bc instanceof VarEqualityConstraint) {
    //                     //         newBc = new VarEqualityConstraint(((VarEqualityConstraint) bc).leftVar(), ((VarEqualityConstraint) bc).rightVar());
    //                     //     } else if (bc instanceof VarInequalityConstraint) {
    //                     //         newBc = new VarInequalityConstraint(((VarInequalityConstraint) bc).leftVar(), ((VarInequalityConstraint) bc).rightVar());
    //                     //     } else if (bc instanceof EqualityConstraint) {
    //                     //         newBc = new EqualityConstraint(((EqualityConstraint) bc).sv(), ((EqualityConstraint) bc).variable());
    //                     //     } else if (bc instanceof InequalityConstraint) {
    //                     //         newBc = new InequalityConstraint(((InequalityConstraint) bc).sv(), ((InequalityConstraint) bc).variable());
    //                     //     } else if (bc instanceof IntegerAssignmentConstraint) {
    //                     //         newBc = new IntegerAssignmentConstraint(((IntegerAssignmentConstraint) bc).sv(), ((IntegerAssignmentConstraint) bc).value());
    //                     //     } else {
    //                     //         throw new FAPEException("Unhandled binding constraint type: " + bc);
    //                     //     }
    //                     //     if (newBc != null) {
    //                     //         newAction.chronicle().addConstraint(newBc);
    //                     //     }
    //                     // }
    //                     break;
                    
    //             }
    //             newPlan.insert(newAction);
    //             for(Task c : this.taskNet.children(original))
    //                 tasks.add(new Pair<>(c, newAction));
    //         } else if (!tasks.isEmpty()) {
    //             Pair<Task,Action> pairToInsert = tasks.poll();
    //             Task newTask = new Task(pairToInsert.value1.name(), pairToInsert.value1.args(), newPlan.pb.context(), refCounter);
    //             if (pairToInsert.value2 == null) {
    //                 newPlan.taskNet.insert(newTask);
    //             } else {
    //                 newPlan.taskNet.insert(newTask, pairToInsert.value2);
    //             }
    //             for(Action c : this.taskNet.children(pairToInsert.value1))
    //                 actions.add(new Pair<>(c, newTask));
    //         }
    //     }
    //     TinyLogger.LogInfo("New Plan:");
    //     TinyLogger.LogInfo(newPlan);
    //     return newPlan;
    // }

    private List<Handler> getHandlers() {
        return pl != null ? pl.getHandlers() : Collections.emptyList();
    }

    public HierarchicalConstraints getHierarchicalConstraints() {
        return getExtension(HierarchicalConstraints.class);
    }

    /**
     * Returns true if the action can be added to the State. This is populated by reasonning on derivabilities,
     * depending on the planner used.
     */
    public boolean isAddable(AbstractAction a) {
        if(addableTemplates == null) {
//            assert addableActions == null : "Addable action where added without generating the related templates.";
            return true; // addable by default
        } else {
            return addableTemplates.contains(a);
        }
    }

    @SuppressWarnings("unchecked")
    public <T> T getExtension(Class<T> clazz) {
        return (T) extensions().filter(ext -> ext.getClass() == clazz)
                .findFirst().orElseThrow(() -> new FAPEException("Extension not found: "+clazz) );
    }
    public void addExtension(StateExtension ext) {
        assert extensions.stream().allMatch(e -> e.getClass() != ext.getClass())
                : "Already an extension with the same class: " + ext;
        extensions.add(ext);
        // replay old signals
        for(Handler.StateLifeTime signal : pastSignals)
            ext.notify(signal);

        for(Chronicle c : pb.chronicles())
            ext.chronicleMerged(c);
        for(Action a : getAllActions())
            ext.chronicleMerged(a.chronicle());
    }
    public Stream<StateExtension> extensions() { return extensions.stream(); }
    @SuppressWarnings("rawtypes")
    public boolean hasExtension(Class clazz) { return extensions().anyMatch(ext -> ext.getClass() == clazz); }

    private List<Handler.StateLifeTime> pastSignals = new ArrayList<>(2);
    public void notify(Handler.StateLifeTime stateLifeTime) {
        for(StateExtension ext : extensions) {
            ext.notify(stateLifeTime);
        }
        pastSignals.add(stateLifeTime);
    }

    /**
     * Returns True if the state is consistent (ie. stn and bindings
     * consistent), False otherwise.
     * This method propagates the constraint neworks
     */
    public boolean checkConsistency() {
        isConsistent &= !isDeadEnd && csp.isConsistent();
        return isConsistent;
    }

    public boolean isConsistent() { return isConsistent; }

    public String report() {
        String ret = "";
        ret += "{\n";
        ret += "  state[" + mID + "]\n";
        ret += "  cons: " + csp.bindings().report() + "\n";
        //ret += "  stn: " + this.csp.stn().report() + "\n";
        ret += "  consumers: " + tdb.getConsumers().size() + "\n";
        for (Timeline b : tdb.getConsumers()) {
            ret += b.Report();
        }
        ret += "\n";
        ret += Printer.taskNetwork(this, taskNet);
        //ret += "  databases: "+this.tdb.report()+"\n";

        ret += "}\n";

        return ret;
    }

    public boolean containsTimelineWithID(int tlID) {
        return tdb.containsTimelineWithID(tlID);
    }

    /**
     * Retrieve the Timeline with the same ID.
     *
     * @param timelineID ID of the timeline to lookup
     * @return The timeline with the same ID.
     */
    public Timeline getTimeline(int timelineID) {
        return tdb.getTimeline(timelineID);
    }

    /**
     * @param s a logical statement to look for.
     * @return the Action containing s. Returns null if no action containing s
     * was found.
     */
    public Optional<Action> getActionContaining(LogStatement s) {
        if(s.container().container().isEmpty()) {
            return Optional.empty();
        } else if(s.container().container().get() instanceof Action) {
            Action a = (Action) s.container().container().get();
            assert a.contains(s);
            assert getAllActions().contains(a);
            return Optional.of(a);
        } else {
            assert getAllActions().stream().noneMatch(a -> a.contains(s));
            return Optional.empty();
        }
    }

    public void breakCausalLink(LogStatement supporter, LogStatement consumer) {
        tdb.breakCausalLink(supporter, consumer);
    }

    /**
     * Remove a statement from the state. It does so by identifying the temporal
     * database in which the statement appears and removing it from the
     * database. If necessary, the database is split in two.
     *
     * @param s Statement to remove.
     */
    private void removeStatement(LogStatement s) {
        tdb.removeStatement(s);
    }

    /**
     * Remove an action from the plan. This should only be done when repairing or replanning.
     * See 5.5.4.1 in the thesis.
     * 
     * @param a Action to remove.
     */
    public void removeAction(Action a) {
        // remove all assertions, constraints, subtasks, timepoints an pending actions refining a subtask
        if (!taskNet.contains(a))
            return;
        for(Task t : taskNet.children(a)) {
            TinyLogger.LogInfo("Removing task: "+t);
            removeTask(t);
        }
        for(LogStatement s : a.chronicle().logStatements()) {
            // TinyLogger.LogInfo("Removing statement: "+s);
            removeStatement(s);
        }
        removeVariables(a.jUsedVariables());
        TinyLogger.LogInfo("Removing action: "+a);
        taskNet.remove(a);
    }

    private void removeTask(Task t) {
        if (!taskNet.contains(t))
            return;
        for (Action subAction : taskNet.children(t)) {
            if (subAction.status() == ActionStatus.PENDING)
                removeAction(subAction);
        }
        removeVariables(t.jUsedVariables());
        taskNet.remove(t);
    }

    private void removeVariables(Set<Variable> vars) {
        for (Variable v : vars) {
            if (v instanceof VarRef) {
                TinyLogger.LogInfo("Removing variable: "+v);
                csp.bindings().removeVariable((VarRef) v);
            }
            if (v instanceof TPRef) {
                // TinyLogger.LogInfo("Removing timepoint: "+v);
                csp.stn().removeTimePoint((TPRef) v);
            }
        }
    }

    /**
     * Return all possible values of a global variable.
     *
     * @param var Reference to a global variable
     * @return All possible values for this variable.
     */
    public Collection<String> possibleValues(VarRef var) {
        assert csp.bindings().contains(var) : "Constraint Network doesn't contains " + var;
        return csp.bindings().domainOf(var);
    }

    /**
     * True if the state variables on which those databases apply are necessarily unified (i.e. same functions and same variables).
     */
    public boolean unified(Timeline a, Timeline b) {
        return unified(a.stateVariable, b.stateVariable);
    }

    /**
     * True iff the state variables are necessarily unified (same functions and same variables).
     */
    public boolean unified(ParameterizedStateVariable a, ParameterizedStateVariable b) {
        if(a.func() != b.func())
            return false;
        for(int i=0 ; i<a.args().length ; i++) {
            if(!unified(a.arg(i), b.arg(i)))
                return false;
        }
        return true;
    }

    /**
     * @return True if both TemporalDatabases might be unifiable (ie. the refer
     * to two unifiable state variables).
     */
    public boolean unifiable(Timeline a, Timeline b) {
        return unifiable(a.stateVariable, b.stateVariable);
    }

    /**
     * Returns true if two state variables are unifiable (ie: they are on the
     * same function and their variables are unifiable).
     */
    public boolean unifiable(ParameterizedStateVariable a, ParameterizedStateVariable b) {
        try {
            if (a.func().equals(b.func())) {
                return unifiable(a.args(), b.args());
            } else {
                return false;
            }
        } catch (Exception e) {
            // System.out.println("Error while unifying " + a + " and " + b);
            throw e;
        }
    }

    /**
     * Tests Unifiability of a sequence of global variables. The two lists must
     * be of the same size.
     *
     * @return True if, for all i in 0..size(as), as[i] and bs[i] are unifiable.
     */
    public boolean unifiable(VarRef[] as, VarRef[] bs) {
        assert as.length == bs.length : "The two lists have different size.";
        for (int i = 0; i < as.length ; i++) {
            try {
                if (!unifiable(as[i], bs[i])) {
                    return false;
                }
            } catch (Exception e) {
                // System.out.println("Error while unifying " + as[i] + " and " + bs[i]);
                throw e;
            }
        }
        return true;
    }

    /**
     * Returns true if the statement s can be an enabler for the database db.
     *
     * Its means e has to be a transition event and that both state variables
     * and the consume/produce values must be unifiable.
     *
     * @param s The logical statement (enabler)
     * @param db the timeline (to be enabled)
     */
    public boolean canBeEnabler(LogStatement s, Timeline db) {
        boolean canSupport = s instanceof Transition || s instanceof Assignment;
        canSupport = canSupport && unifiable(s.sv(), db.stateVariable);
        canSupport = canSupport && unifiable(s.endValue(), db.getGlobalConsumeValue());
        return canSupport;
    }

    /**
     * Insert an action into the state, applying all needed modifications.
     *
     * @param act Action to insert.
     */
    public void insert(Action act) {
        taskNet.insert(act);

        apply(act.chronicle());
        // System.out.println(act.chronicle());

        // make sure that this action is executed after earliest execution.
        csp.stn().enforceMinDelay(pb.start(), act.start(), getEarliestExecution());

        for(Handler h : getHandlers())
            h.actionInserted(act, this, pl);

        csp.isConsistent();
    }

    public boolean apply(PartialPlanModification mod, boolean isFastForwarding) { //TODO, simply this fast forwarding knowledge
        modifications.add(mod);
        mod.apply(this, isFastForwarding);
        return isConsistent();
    }

    /**
     * Applies all pending modifications of the problem. A problem comes with a
     * sequence of StateModifiers that depict the current status of the problem.
     * This method simply applies all chronicles that were not previously
     * applied.
     */
    public void update() {
        if (problemRevision == -1) {
            csp.stn().recordTimePointAsStart(pb.start());
            csp.stn().recordTimePointAsEnd(pb.end());
            csp.stn().recordTimePoint(pb.earliestExecution());
            csp.stn().enforceBefore(pb.start(), pb.earliestExecution());
        }
        for (int i = problemRevision + 1; i < pb.chronicles().size(); i++) {
            PartialPlanModification mod = new ChronicleInsertion(pb.chronicles().get(i));
            apply(mod, false);
            problemRevision = i;
        }
    }

    /**
     * Inserts a logical statement into a state
     *
     * @param s Statement to insert
     */
    private void apply(LogStatement s) {
        assert !s.sv().func().isConstant() : "LogStatement on a constant function: "+s;
        if(s.isChange())
            csp.stn().enforceStrictlyBefore(s.start(), s.end());
        else
            csp.stn().enforceBefore(s.start(), s.end());
        tdb.addNewTimeline(s);
    }

    /**
     * Enforce a binding constraint in the state.
     *
     * Those have effect on the constraint network only.
     * @param bc BindingConstraint to be enforced.
     */
    public void apply(Chronicle chronicle, BindingConstraint bc) {
        if (bc instanceof AssignmentConstraint) {
            AssignmentConstraint c = (AssignmentConstraint) bc;
            List<String> values = new LinkedList<>();
            for (VarRef v : c.sv().args()) {
                assert v instanceof InstanceRef : "Value "+v+" of "+bc+" is not an instance";
                values.add(((InstanceRef) v).instance());
            }
            assert c.variable() instanceof InstanceRef : "Left value of "+bc+" is not an instance";
            values.add(((InstanceRef) c.variable()).instance());
            csp.bindings().addAllowedTupleToNAryConstraint(c.sv().func().name(), values);
        } else if (bc instanceof VarEqualityConstraint) {
            VarEqualityConstraint c = (VarEqualityConstraint) bc;
            csp.bindings().AddUnificationConstraint(c.leftVar(), c.rightVar());
        } else if (bc instanceof VarInequalityConstraint) {
            VarInequalityConstraint c = (VarInequalityConstraint) bc;
            csp.bindings().addSeparationConstraint(c.leftVar(), c.rightVar());
        } else if (bc instanceof EqualityConstraint) {
            EqualityConstraint c = (EqualityConstraint) bc;
            assert csp.bindings().isRecorded(c.variable()) : "Variable "+c.variable()+" is not recorded in the CSP.";
            List<VarRef> variables = new LinkedList<>(Arrays.asList(c.sv().args()));
            variables.add(c.variable());
            csp.bindings().addNAryConstraint(variables, c.sv().func().name());
        } else if (bc instanceof InequalityConstraint) {
            // create a new value tmp such that
            // c.sv == tmp and tmp != c.variable
            InequalityConstraint c = (InequalityConstraint) bc;
            List<VarRef> variables = new LinkedList<>(Arrays.asList(c.sv().args()));
            VarRef tmp = new VarRef(c.sv().func().valueType(), refCounter, new Label(chronicle.getLabel(),""));
            csp.bindings().addVariable(tmp);
            variables.add(tmp);
            csp.bindings().addNAryConstraint(variables, c.sv().func().name());
            csp.bindings().addSeparationConstraint(tmp, c.variable());
        } else if (bc instanceof IntegerAssignmentConstraint) {
            IntegerAssignmentConstraint c = (IntegerAssignmentConstraint) bc;
            List<String> values = new LinkedList<>();
            for (VarRef v : c.sv().args()) {
                assert v instanceof InstanceRef;
                values.add(((InstanceRef) v).instance());
            }
            csp.bindings().addPossibleValue(c.value());
            csp.bindings().addAllowedTupleToNAryConstraint(c.sv().func().name(), values, c.value());
        } else if(bc instanceof InConstraint) {
            csp.bindings().addConstraint(
                    new InSetConstraint(((InConstraint) bc).leftVar(), ((InConstraint) bc).rightVars()));
        } else {
            throw new FAPEException("Unhandled constraint type: "+bc);
        }
    }

    /**
     * Inserts a resource statement into a state.
     *
     * @param s Statement to insert
     */
    private void apply(ResourceStatement s) {
        throw new RuntimeException("Resource statements are not supported.");
    }

    /**
     * Inserts a statement into a state
     *
     * @param mod StateModifier in which the statement appears
     * @param s Statement to insert
     */
    private void apply(Chronicle mod, Statement s) {
        if (s instanceof LogStatement) {
            apply((LogStatement) s);
        } else if (s instanceof ResourceStatement) {
            apply((ResourceStatement) s);
        } else {
            throw new FAPEException("Unsupported statement: " + s);
        }
    }

    /**
     * Applies the modification implied by a temporal constraint. All time
     * points referenced in the constraint must have been previously recorded in
     * the STN.
     *
     * @param mod StateModifier in which the constraint appears.
     * @param tc The TemporalConstraint to insert.
     */
    private void apply(Chronicle mod, TemporalConstraint tc) {
        try {
            if(tc instanceof MinDelayConstraint) {
                csp.addMinDelay(tc.src(), tc.dst(), translateToCSPVariable(((MinDelayConstraint) tc).minDelay()));
            } else if(tc instanceof ContingentConstraint) {
                ContingentConstraint cc = (ContingentConstraint) tc;
                csp.addContingentConstraint(
                        cc.src(), cc.dst(),
                        translateToCSPVariable(cc.min()),
                        translateToCSPVariable(cc.max()));
            } else {
                throw new UnsupportedOperationException("Temporal contrainst: "+tc+" is not supported yet.");
            }
        } catch (InconsistentTemporalNetwork e) {
            throw new InconsistentTemporalNetwork("Inconsistent temporal network when applying constraint "+tc + " for " + mod, e);
        }
    }

    public void apply(Chronicle chronicle) {
        // for every instance declaration, create a new CSP Var with itself as domain
        for (String instanceName : chronicle.instances()) {
            csp.bindings().addPossibleValue(instanceName);
            csp.bindings().addVariable(pb.instances().referenceOf(instanceName), Collections.singletonList(instanceName));
        }

        // Declare new variables to the constraint network.
        for (VarRef var : chronicle.vars()) {
            csp.bindings().addVariable(var);
        }

        for(BindingConstraint bc : chronicle.bindingConstraints())
            apply(chronicle, bc);

        // apply all temporal constraints
        for (TemporalConstraint tc : chronicle.temporalConstraints())
            apply(chronicle, tc);

        // needs time points to be defined
        for(Task t : chronicle.tasks()) {
            enforceBefore(t.start(), t.end());
            enforceBefore(pb.start(), t.start());

            if(chronicle.container().nonEmpty() && chronicle.container().get() instanceof Action)
                taskNet.insert(t, (Action) chronicle.container().get());
            else
                taskNet.insert(t);

            for(Handler h : getHandlers())
                h.taskInserted(t, this, pl);
        }

        // needs its timepoints to be defined
        for (Statement ts : chronicle.statements()) {
            apply(chronicle, ts);
        }

        for(StateExtension ext : extensions) {
            ext.chronicleMerged(chronicle);
        }
    }
    public void addSupportConstraint(ChainComponent cc, Action act) {
        supportConstraints.removeIf(p -> p.value1 == cc.mID);
        supportConstraints.add(new Pair<>(cc.mID, act));
    }

    public void timelineAdded(Timeline a) {
        for(StateExtension ext : extensions)
            ext.timelineAdded(a);
    }

    public void timelineExtended(Timeline tl) {
        for(StateExtension ext : extensions)
            ext.timelineExtended(tl);
        assert !tl.isConsumer() || tdb.getConsumers().contains(tl);
    }

    public void timelineRemoved(Timeline tl) {
        for(StateExtension ext : extensions)
            ext.timelineRemoved(tl);
    }

    /**
     * Retrieves all valid resolvers for this unsupported timeline.
     *
     * As a side effects, this method also cleans up the resolvers stored in this state to remove double entries
     * and supporters that are not valid anymore.
     */
    public List<Resolver> getResolversForOpenGoal(Timeline og, PlanningOptions.ActionInsertionStrategy actionInsertionStrategy) {
        assert og.isConsumer();
        assert tdb.getConsumers().contains(og) : "This timeline is not an open goal.";

        return getExtension(OpenGoalSupportersCache.class).getResolversForOpenGoal(og, actionInsertionStrategy);
    }


    public List<Flaw> getAllThreats() {
        return getExtension(ThreatsCache.class).getAllThreats();
    }

    /** Returns true if this State has no flaws */
    public boolean isSolution(List<FlawFinder> finders) {
        for(FlawFinder ff : finders) {
            if(!ff.getFlaws(this, pl).isEmpty())
                return false;
        }
        return true;
    }

    /**
     * Returns the best flaw in this state.
     * Flaws are identified using the provided finders and sorted with the provided comparator.
     * An empty result indicates that the plan has no flaw.
     */
    public Optional<Flaw> getBestFlaw(List<FlawFinder> finders, Comparator<Flaw> comparator) {
        List<Flaw> flaws = new ArrayList<>();

        for (FlawFinder fd : finders)
            flaws.addAll(fd.getFlaws(this, pl));

        if(flaws.isEmpty())
            return Optional.empty();

        Flaw candidate = null;
        int bestNumResolvers = Integer.MAX_VALUE;

        for(Flaw f : flaws) {
            int num = f.getNumResolvers(this, pl);
            // no resolvers: a dead end exit early
            if(num == 0)
                return Optional.of(f);

            if(num < bestNumResolvers) {
                candidate = f;
                bestNumResolvers = num;
            }
        }
        assert bestNumResolvers > 0;
        assert candidate != null;

        // make candidate the best flaw according to comparator
        for(Flaw f : flaws) {
            if(f == candidate)
                continue;
            if(bestNumResolvers == 1) {
                // we know that the best flaw will have a single resolver but we still want to
                // use the comparator to eliminate non determinism
                int numRes = f.getNumResolvers(this, pl);
                if(numRes > 1)
                    continue;
            }
            if(comparator.compare(f, candidate) < 0)
                candidate = f;
        }
        return Optional.of(candidate);
    }

    public int getMakespan() {
        return getAllActions().stream().mapToInt(a -> getEarliestStartTime(a.end())).max().orElse(0);
    }

    public TimedCanvas getCanvasOfActions() {
        List<Action> acts = new LinkedList<>(getAllActions());
        Collections.sort(acts, (Action a1, Action a2) ->
                getEarliestStartTime(a1.start()) - getEarliestStartTime(a2.start()));
        List<ChartLine> lines = new LinkedList<>();

        for (Action a : acts) {
            int start = getEarliestStartTime(a.start());
            int earliestEnd = getEarliestStartTime(a.end());
            String name = Printer.action(this, a);
            TextLabel label = new TextLabel(name, "action-name");

            switch (a.status()) {
                case EXECUTED:
                    lines.add(new ChartLine(label, new RectElem(start, earliestEnd - start, "successful")));
                    break;
                case EXECUTING:
                case PENDING:
                    if (getDurationBounds(a).nonEmpty()) {
                        int min = getDurationBounds(a).get()._1();
                        int max = getDurationBounds(a).get()._2();
                        lines.add(new ChartLine(label,
                                new RectElem(start, min, "pending"),
                                new RectElem(start + min + 0.1f, max - min, "uncertain")));
                    } else {
                        lines.add(new ChartLine(label, new RectElem(start, earliestEnd - start, "pending")));
                    }
                    break;
                case FAILED:
            }
        }
        return new TimedCanvas(lines, getEarliestStartTime(pb.earliestExecution()));
    }

    /*** Wrapper around STN ******/

    public void enforceBefore(TPRef a, TPRef b) { csp.stn().enforceBefore(a, b); }

    public void enforceBefore(Collection<TPRef> as, TPRef b) {
        for(TPRef a : as)
            enforceBefore(a, b);
    }

    public void enforceBefore(TPRef a, Collection<TPRef> bs) {
        for(TPRef b : bs)
            enforceBefore(a, b);
    }

    public void enforceStrictlyBefore(TPRef a, TPRef b) { csp.stn().enforceStrictlyBefore(a, b); }

    public boolean canBeBefore(Timeline tl1, Timeline tl2) {
        if(!tl1.hasSinglePersistence()) {
            if (tl2.hasSinglePersistence()) {
                return canAllBeBefore(tl1.getSupportTimePoint(), tl2.getFirstTimePoints());
            } else {
                assert tl2.get(0).change : "First statement of timeline containing changes is a persistence";
                return //canAllBeBefore(tl1.getSupportTimePoint(), tl2.getFirstTimePoints()) &&
                        canAllBeBefore(tl1.getLastTimePoints(), tl2.getFirstChangeTimePoint());
            }
        } else {
            assert !tl2.hasSinglePersistence() : "Temporal comparison of two timelien with persistences only is not supported. " +
                    "Usually this case should have been filtered out.";
            return canAllBeBefore(tl1.getLastTimePoints(), tl2.getFirstChangeTimePoint());
        }
    }

    public void enforceStrictlyBefore(Collection<TPRef> as, TPRef b) {
        for(TPRef a : as)
            enforceStrictlyBefore(a, b);
    }

    public void enforceStrictlyBefore(TPRef a, Collection<TPRef> bs) {
        for(TPRef b : bs)
            enforceStrictlyBefore(a, b);
    }

    public boolean canBeBefore(TPRef a, TPRef b) { return csp.stn().canBeBefore(a, b); }

    /** Returns true if any time point in as can be before a time point in bs */
    public boolean canAnyBeBefore(Collection<TPRef> as, Collection<TPRef> bs) {
        for(TPRef a : as) {
            for(TPRef b : bs) {
                if(canBeBefore(a, b))
                    return true;
            }
        }
        return false;
    }

    /** Returns true if any time point in as can be before b */
    public boolean canAnyBeBefore(Collection<TPRef> as, TPRef b) {
        for(TPRef a : as) {
            if(canBeBefore(a, b))
                return true;
        }
        return false;
    }

    /** Returns true if a can be before a time point in bs */
    public boolean canAnyBeBefore(TPRef a, Collection<TPRef> bs) {
        for(TPRef b : bs) {
            if(canBeBefore(a, b))
                return true;
        }
        return false;
    }

    /** Returns true if any time point in as can be before b */
    public boolean canAnyBeStrictlyBefore(Collection<TPRef> as, TPRef b) {
        for(TPRef a : as) {
            if(canBeStrictlyBefore(a, b))
                return true;
        }
        return false;
    }

    /** Returns true if a can be before a time point in bs */
    public boolean canAnyBeStrictlyBefore(TPRef a, Collection<TPRef> bs) {
        for(TPRef b : bs) {
            if(canBeStrictlyBefore(a, b))
                return true;
        }
        return false;
    }

    /** Returns true if all time point in as can be before all time points in bs */
    public boolean canAllBeBefore(Collection<TPRef> as, Collection<TPRef> bs) {
        for(TPRef a : as) {
            for(TPRef b : bs) {
                if(!canBeBefore(a, b))
                    return false;
            }
        }
        return true;
    }

    /** Returns true if all time point in as can be before b */
    public boolean canAllBeBefore(Collection<TPRef> as, TPRef b) {
        for(TPRef a : as) {
            if(!canBeBefore(a, b))
                return false;
        }
        return true;
    }

    /** Returns true if a can be before all time point in bs */
    public boolean canAllBeBefore(TPRef a, Collection<TPRef> bs) {
        for(TPRef b : bs) {
            if(!canBeBefore(a, b))
                return false;
        }
        return true;
    }

    public boolean mustOverlap(Collection<TPRef> firstIntervalStart, Collection<TPRef> firstIntervalEnd,
                               Collection<TPRef> secondIntervalStart, Collection<TPRef> secondIntervalEnd) {
        return !canAllBeBefore(firstIntervalEnd, secondIntervalStart) || !canAllBeBefore(secondIntervalEnd, firstIntervalStart);
    }

    public boolean canBeStrictlyBefore(TPRef a, TPRef b) { return csp.stn().canBeStrictlyBefore(a, b); }

    public boolean enforceConstraint(TPRef a, TPRef b, int min, int max) { return csp.stn().enforceConstraint(a, b, min, max); }

    public void enforceDelay(TPRef a, TPRef b, int delay) { csp.stn().enforceMinDelay(a, b, delay); }

    public int getEarliestStartTime(TPRef a) { return csp.stn().getEarliestTime(a); }
    public int getMaxEarliestStartTime(List<TPRef> as) {
        int max = 0;
        for(TPRef a : as) {
            int x = getEarliestStartTime(a);
            if(x > max)
                max = x;
        }
        return max;
    }
    public int getLatestStartTime(TPRef a) { return csp.stn().getLatestTime(a); }

    public boolean checksDynamicControllability() { return csp.stn().checksDynamicControllability(); }

    public Option<Tuple2<Integer,Integer>> getDurationBounds(Action a) {
        return csp.stn().contingentDelay(a.start(), a.end());
    }


    /********* Wrapper around the task network **********/

    /** Returns a set of all ground actions this lifted action might be instantiated to */
    public IRSet<GAction> getGroundActions(Action lifted) {
        assert csp.bindings().isRecorded(lifted.instantiationVar());
        Domain dom = csp.bindings().rawDomain(lifted.instantiationVar());
        return new IRSet<>(pl.preprocessor.store.getIntRep(GAction.class), dom.toBitSet());
    }

    /** Returns all ground versions of this statements */
    public Set<GAction.GLogStatement> getGroundStatements(LogStatement s) {
        // action by which this statement was introduced (null if no action)
        Optional<Action> containingAction = getActionContaining(s);

        if (!containingAction.isPresent()) { // statement was not added as part of an action
            int minDur = csp.stn().getMinDelay(s.start(), s.end());
            if(!s.needsSupport()) {
                assert s instanceof Assignment;
                assert s.endValue() instanceof InstanceRef;
                Collection<Fluent> fluents = DisjunctiveFluent.fluentsOf(s.sv(), s.endValue(), this, pl);
                return fluents.stream()
                        .map(f -> new GAction.GAssignment(
                                pl.preprocessor.getFluent(f.sv, f.value),
                                minDur, null, Optional.empty()))
                        .collect(Collectors.toSet());
            } else if(s.isChange()) {
                assert s instanceof Persistence;
                Collection<Fluent> fluents = DisjunctiveFluent.fluentsOf(s.sv(), s.endValue(), this, pl);
                Collection<InstanceRef> startValues = valuesOf(s.startValue());

                return fluents.stream()
                        .flatMap(f -> startValues.stream()
                                .map(startVal -> new GAction.GTransition(
                                        pl.preprocessor.getFluent(f.sv, startVal),
                                        pl.preprocessor.getFluent(f.sv, f.value),
                                        minDur, null, Optional.empty())))
                        .collect(Collectors.toSet());
            } else {
                assert s instanceof Persistence;
                Collection<Fluent> fluents = DisjunctiveFluent.fluentsOf(s.sv(), s.endValue(), this, pl);

                return fluents.stream()
                        .map(f -> new GAction.GPersistence(
                                pl.preprocessor.getFluent(f.sv, f.value),
                                minDur, null, Optional.empty()))
                        .collect(Collectors.toSet());
            }
        } else { // statement was added as part of an action or a decomposition
            Collection<GAction> acts = getGroundActions(containingAction.get());

            // local reference of the statement, used to extract the corresponding ground statement from the GAction
            assert containingAction.get().context().contains(s);
            LStatementRef statementRef = containingAction.get().context().getRefOfStatement(s);

            return acts.stream()
                    .map(ga -> ga.statementWithRef(statementRef))
                    .collect(Collectors.toSet());
        }
    }

    public List<InstanceRef> valuesOf(VarRef var) {
        List<InstanceRef> values = new ArrayList<>();
        for(String val : domainOf(var)) {
            values.add(pb.instances().referenceOf(val));
        }
        return values;
    }

    public List<Action> getAllActions() { return taskNet.getAllActions(); }

    public List<Task> getOpenTasks() { return taskNet.getOpenTasks(); }

    public List<Action> getUnmotivatedActions() { return taskNet.getNonSupportedMotivatedActions(); }

    /**
     *  Unifies the time points of the action condition and those of the action, and add
     * the support link in the task network.
     */
    public void addSupport(Task task, Action act) {
        assert task.name().equals(act.taskName());
        csp.stn().enforceConstraint(task.start(), act.start(), 0, 0);
        csp.stn().enforceConstraint(task.end(), act.end(), 0, 0);

        for(int i=0 ; i<task.args().size() ; i++) {
            csp.bindings().AddUnificationConstraint(task.args().get(i), act.args().get(i));
        }

        taskNet.addSupport(task, act);
        for(Handler h : getHandlers())
            h.supportLinkAdded(act, task, this);
    }

    public int getNumActions() { return taskNet.getNumActions(); }

    public int getNumRoots() { return taskNet.getNumRoots(); }

    public void exportTaskNetwork(String filename) { taskNet.exportToDot(this, filename); }

    /******** Wrapper around the constraint network ***********/

    public boolean canSupport(Action act, Task task) {
        if(!act.taskName().equals(task.name()))
            return false;
        assert act.args().size() == task.args().size();
        for(int i=0 ; i<act.args().size() ; i++)
            if(!unifiable(act.args().get(i), task.args().get(i)))
                return false;

        return canBeBefore(act.start(), task.start()) &&
                canBeBefore(task.start(), act.start()) &&
                canBeBefore(act.end(), task.end()) &&
                canBeBefore(task.end(), act.end());
    }

    public List<String> domainOf(VarRef var) { return csp.bindings().domainOf(var); }

    public int domainSizeOf(VarRef var) { 
        try {
            
            return csp.bindings().domainSize(var); 
        } catch (Exception e) {
            // TODO: handle exception
            // System.out.println("Error while getting domain size of variable " + var + " with type " + var.getType());
            throw e;
        }
    }

    public void addUnificationConstraint(VarRef a, VarRef b) { csp.bindings().AddUnificationConstraint(a, b); }

    public void addUnificationConstraint(ParameterizedStateVariable a, ParameterizedStateVariable b) {
        assert a.func() == b.func() : "Error unifying two state variable on different functions.";
        assert a.args().length == b.args().length : "Error different number of arguments.";
        for (int i = 0; i < a.args().length; i++) {
            csp.bindings().AddUnificationConstraint(a.arg(i), b.arg(i));
        }
    }

    public void addSeparationConstraint(VarRef a, VarRef b) { csp.bindings().addSeparationConstraint(a, b); }

    public void restrictDomain(VarRef var, Collection<String> values) { csp.bindings().restrictDomain(var, values); }

    public void bindVariable(VarRef var, String value) {
        List<String> values = new LinkedList<>();
        values.add(value);
        restrictDomain(var, values);
    }

    public List<VarRef> getUnboundVariables() { return csp.bindings().getUnboundVariables(); }

    public boolean unified(VarRef a, VarRef b) { return csp.bindings().unified(a, b); }

    public boolean unifiable(VarRef a, VarRef b) { return csp.bindings().unifiable(a, b); }

    public boolean separable(VarRef a, VarRef b) { return csp.bindings().separable(a, b); }

    public void addValuesToValuesSet(String setID, List<String> values) { csp.bindings().addAllowedTupleToNAryConstraint(setID, values);}

    public void addValuesSetConstraint(List<VarRef> variables, String setID) { csp.bindings().addNAryConstraint(variables, setID);}

    /**
     * Retrieves the CSP variable representing a cosntant parameterized state variable.
     * If necessary, this variable is created and added to the CSP.
     */
    public VarRef getVariableOf(ParameterizedStateVariable sv) {
        assert sv.func().isConstant();
        if(!stateVarsToVariables.containsKey(sv)) {
            assert sv.func().valueType().isNumeric() : "Temporal constraint involving the non-integer function: " + sv.func();
            VarRef variable = new VarRef(sv.func().valueType(), refCounter, new Label("",sv.toString()));
            csp.bindings().addIntVariable(variable);
            List<VarRef> variablesOfNAryConst = new ArrayList<>(Arrays.asList(sv.args()));
            variablesOfNAryConst.add(variable);
            csp.bindings().addNAryConstraint(variablesOfNAryConst, sv.func().name());
            stateVarsToVariables.put(sv, variable);
        }
        return stateVarsToVariables.get(sv);
    }

    IntExpression translateToCSPVariable(IntExpression expr) {
        Function<IntExpression,IntExpression> transformation = e -> {
            if(e instanceof StateVariable) {
                ParameterizedStateVariable sv = ((StateVariable) e).sv();
                assert sv.func().valueType().isNumeric();
                return IntExpression.variable(getVariableOf(sv), e.lb(), e.ub());
            } else {
                assert !(e instanceof LStateVariable) : "Error: local state variable was not binded";
                return e;
            }
        };
        return expr.jTrans(transformation);
    }

    /************ Wrapper around TemporalDatabaseManager **********************/

    public Iterable<Timeline> getTimelines() { return tdb.getTimelines(); }

    public void insertTimelineAfter(Timeline supporter, Timeline consumer, ChainComponent precedingComponent) {
        tdb.insertTimelineAfter(this, supporter, consumer, precedingComponent);
    }
}
