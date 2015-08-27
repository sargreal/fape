package fape.core.planning.planninggraph;

import fape.core.inference.HReasoner;
import fape.core.inference.Predicate;
import fape.core.inference.Term;
import fape.core.planning.grounding.*;
import fape.core.planning.heuristics.DefaultIntRepresentation;
import fape.core.planning.planner.APlanner;
import fape.core.planning.states.State;
import fape.core.planning.timelines.Timeline;
import planstack.anml.model.LVarRef;
import planstack.anml.model.abs.AbstractAction;
import planstack.anml.model.concrete.*;
import planstack.constraints.bindings.Domain;

import java.util.*;
import java.util.stream.Collectors;

public class FeasibilityReasoner {

    final APlanner planner;
    private Set<GAction> allActions;

    /** Maps ground actions from their ID */
    public final HashMap<Integer, GAction> gactions = new HashMap<>();

    final HReasoner<Term> baseReasoner;

    public FeasibilityReasoner(APlanner planner, State initialState) {
        this.planner = planner;
        // this Problem contains all the ground actions
        GroundProblem base = planner.preprocessor.getGroundProblem();

        // record all n ary constraints (action instantiations and task supporters)
        Set<String> recordedTask = new HashSet<>();
        for(AbstractAction aa : planner.pb.abstractActions()) {
            initialState.csp.bindings().recordEmptyNAryConstraint(aa.name(), true, aa.allVars().length+1);
            initialState.csp.bindings().addPossibleValue(aa.name());
            if(!recordedTask.contains(aa.taskName())) {
                initialState.csp.bindings().recordEmptyNAryConstraint(aa.taskName(), true, aa.args().size()+2);
                recordedTask.add(aa.taskName());
            }
        }

        allActions = new HashSet<>(base.gActions);

        baseReasoner = new HReasoner<>(new DefaultIntRepresentation<>()); // TODO: a specialized int representation would be much more efficient
        for(GAction ga : allActions) {
            ga.addClauses(baseReasoner);
        }

        // all clauses have been added, lock the reasoner for a better sharing of data structures (clauses)
        baseReasoner.lock();

        for(GAction ga : allActions) {
            initialState.csp.bindings().addPossibleValue(ga.id);
            assert(!gactions.containsKey(ga.id));
            gactions.put(ga.id, ga);
        }

        allActions = getAllActions(initialState);
        base.gActions.clear();
        base.gActions.addAll(allActions);

        for(GAction ga : allActions) {
            // values for all variables of this action
            List<String> values = new LinkedList<>();
            for(LVarRef var : ga.variables)
                values.add(ga.valueOf(var).instance());
            // add possible tuple to instantiation constraints
            initialState.csp.bindings().addAllowedTupleToNAryConstraint(ga.abs.name(), values, ga.id);

            // values for arguments of this action
            List<String> argValues = new LinkedList<>();
            for(LVarRef var : ga.abs.args())
                argValues.add(ga.valueOf(var).instance());
            argValues.add(ga.abs.name());
            // add possible tuple to supporter constraints
            initialState.csp.bindings().addAllowedTupleToNAryConstraint(ga.abs.taskName(), argValues, ga.id);
        }
    }

    public Set<GAction> getAllActions(State st) {
        if(st.addableGroundActions != null)
            return st.addableGroundActions;

        HReasoner<Term> r = getReasoner(st);
        HashSet<GAction> feasibles = new HashSet<>();
        for(Term t : r.trueFacts()) {
            if(t instanceof Predicate && ((Predicate) t).name.equals(Predicate.PredicateName.POSSIBLE_IN_PLAN)) //TODO make a selector for this
                feasibles.add((GAction) ((Predicate) t).var);
        }

        List<Integer> allowedDomainOfActions = new LinkedList<>();
        for(GAction ga : feasibles) {
            allowedDomainOfActions.add(ga.id);
        }
        Domain dom = st.csp.bindings().intValuesAsDomain(allowedDomainOfActions);
        for(Action a : st.getAllActions()) {
            st.csp.bindings().restrictDomain(a.instantiationVar(), dom);
        }

        st.addableGroundActions = feasibles;
        return feasibles;
    }

    public HReasoner<Term> getReasoner(State st) {
        return getReasoner(st, allActions);
    }

    public Collection<GTaskCond> getGroundedTasks(Task liftedTask, State st) {
        List<GTaskCond> tasks = new LinkedList<>();
        LinkedList<List<InstanceRef>> varDomains = new LinkedList<>();
        for(VarRef v : liftedTask.args()) {
            varDomains.add(new LinkedList<InstanceRef>());
            for(String value : st.domainOf(v)) {
                varDomains.getLast().add(st.pb.instance(value));
            }
        }
        List<List<InstanceRef>> instantiations = PGUtils.allCombinations(varDomains);
        for(List<InstanceRef> instantiation : instantiations) {
            GTaskCond task = new GTaskCond(liftedTask.name(), instantiation);
            tasks.add(task);
        }
        return tasks;
    }

    public Iterable<GTaskCond> getDerivableTasks(State st) {
        List<GTaskCond> derivableTasks = new LinkedList<>();
        for(Task ac : st.getOpenTasks()) {
            derivableTasks.addAll(getGroundedTasks(ac, st));
        }

        return derivableTasks;
    }

    private HReasoner<Term> getReasoner(State st, Collection<GAction> acceptable) {
        if(st.reasoner != null)
            return st.reasoner;

        HReasoner<Term> r = new HReasoner<>(baseReasoner, true);
        for(Fluent f : planner.preprocessor.getGroundProblem().allFluents(st)) {
            // if the fluent is not already recorded, it does not appear in any clause and we can safely ignore it
            if(r.hasTerm(f))
                r.set(f);
        }

        for(GAction acc : acceptable)
            r.set(new Predicate(Predicate.PredicateName.ACCEPTABLE, acc));

        for(GTaskCond tc : getDerivableTasks(st)) {
            r.set(new Predicate(Predicate.PredicateName.DERIVABLE_TASK, tc));
        }

        for(Action a : st.getAllActions()) {
            for(GAction ga : getGroundActions(a, st)) {
                r.set(new Predicate(Predicate.PredicateName.IN_PLAN, ga));
            }
        }

        Set<GAction> feasibles = new HashSet<>();

        for(Term t : r.trueFacts()) {
            if(t instanceof Predicate && ((Predicate) t).name.equals(Predicate.PredicateName.POSSIBLE_IN_PLAN)) //TODO make a selector for this
                feasibles.add((GAction) ((Predicate) t).var);
        }

        // continue until a fixed point is reached
        if(feasibles.size() < acceptable.size()) {
            // number of possible actions was reduced, keep going
            return getReasoner(st, feasibles);
        } else {
            // fixed point reached
            st.reasoner = r;
            return r;
        }
    }

    public boolean checkFeasibility(State st) {
        Set<GAction> acts = getAllActions(st);

        for(Action a : st.getUnmotivatedActions()) {
            boolean derivable = false;
            for(GAction ga : getGroundActions(a, st)) {
                if (acts.contains(ga)) {
                    derivable = true;
                    break;
                }
            }
            if(!derivable) {
                // this unmotivated action cannot be derived from the current HTN
                return false;
            }
        }

        for(Action a : st.getAllActions()) {
            boolean feasibleAct = false;
            for(GAction ga : getGroundActions(a, st)) {
                if(st.reasoner.isTrue(new Predicate(Predicate.PredicateName.POSSIBLE_IN_PLAN, ga))) {
                    feasibleAct = true;
                    break;
                }

            }
            if(!feasibleAct) {
                // there is no feasible ground versions of this action
                return false;
            }
        }

        // check that all open goals has at least one achievable fluent
        for(Timeline cons : st.tdb.getConsumers()) {
            boolean supported = false;
            for(Fluent f : DisjunctiveFluent.fluentsOf(cons.stateVariable, cons.getGlobalConsumeValue(), st, planner)) {
                if(st.reasoner.hasTerm(f) && st.reasoner.isTrue(f)) {
                    supported = true;
                    break;
                }
            }
            if(!supported)
                return false;
        }

        // computes the set of non addable actions (used to prune resolvers)
        // TODO: use derivable? (here we are implicitly using all possible_in_plan)
        Set<AbstractAction> addableActions = new HashSet<>();
        for(GAction ga : getAllActions(st))
            addableActions.add(ga.abs);
        Set<AbstractAction> nonAddable = new HashSet<>(st.pb.abstractActions());
        nonAddable.removeAll(addableActions);
        st.notAddable = nonAddable;

        return true;
    }

    public Set<GAction> getGroundActions(Action liftedAction, State st) {
        Set<GAction> ret = new HashSet<>();
        assert st.csp.bindings().isRecorded(liftedAction.instantiationVar());
        for(Integer i : st.csp.bindings().domainOfIntVar(liftedAction.instantiationVar())) {
            if(gactions.containsKey(i)) // the domain might contain any int variable
                ret.add(gactions.get(i));
        }

        return ret;
    }

    /** This will associate with an action a variable in the CSP representing its
     * possible ground versions.
     * @param act Action for which we need to create the variable.
     * @param st  State in which the action appears (needed to update the CSP)
     */
    public void createActionInstantiationVariable(Action act, State st) {
        assert !st.csp.bindings().isRecorded(act.instantiationVar()) : "The action already has a variable for its ground versions.";

        // all ground versions of this actions (represented by their ID)
        LVarRef[] vars = act.abs().allVars();
        List<VarRef> values = new ArrayList<>();
        for(LVarRef v : vars) {
            values.add(act.context().getDefinition(v)._2());
        }

        // Variable representing the ground versions of this action
        st.csp.bindings().AddIntVariable(act.instantiationVar());
        values.add(act.instantiationVar());
        st.addValuesSetConstraint(values, act.abs().name());

        assert st.csp.bindings().isRecorded(act.instantiationVar());
    }

    public void createTaskSupportersVariables(Task task, State st) {
        assert !st.csp.bindings().isRecorded(task.methodSupportersVar());
        assert !st.csp.bindings().isRecorded(task.groundSupportersVar());
        Collection<String> supportingMethods = planner.pb.getSupportersForTask(task.name()).stream()
                .map(aa -> aa.name()).collect(Collectors.toList());

        st.csp.bindings().AddVariable(task.methodSupportersVar(), supportingMethods, "Methods");
        st.csp.bindings().AddIntVariable(task.groundSupportersVar());

        List<VarRef> variables = new LinkedList<>();
        variables.addAll(task.args());
        variables.add(task.methodSupportersVar());
        variables.add(task.groundSupportersVar());
        st.addValuesSetConstraint(variables, task.name());
    }
}
