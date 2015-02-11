package fape.core.planning.search.flaws.resolvers;

import fape.core.planning.planner.APlanner;
import fape.core.planning.states.State;
import fape.core.planning.temporaldatabases.TemporalDatabase;
import planstack.anml.model.abs.AbstractDecomposition;
import planstack.anml.model.concrete.Action;
import planstack.anml.model.concrete.Decomposition;
import planstack.anml.model.concrete.Factory;

/**
 * This resolver decomposes an action in order to support an open goal.
 * No causal link is added, instead a constraint is added stating that
 * the supporter for this open goal must be introduced by this decomposition
 * (or any child action/decomposition)
 */
public class SupportingActionDecomposition extends Resolver {

    /** Action to decompose */
    public final Action act;

    /** ID of the decomposition to apply */
    public final int decID;

    public final int consumerID;

    public SupportingActionDecomposition(Action a, int id, TemporalDatabase consumer) {
        act = a;
        decID = id;
        this.consumerID = consumer.mID;
    }

    @Override
    public boolean hasDecomposition() { return true; }
    @Override
    public Action actionToDecompose() { return act; }

    @Override
    public boolean apply(State st, APlanner planner) {
        final TemporalDatabase consumer = st.GetDatabase(consumerID);
        assert consumer != null : "Could not find consumer.";
        // Apply the i^th decomposition of o.actionToDecompose, where i is given by
        // o.decompositionID

        // Abstract version of the decomposition
        AbstractDecomposition absDec = act.decompositions().get(decID);

        // Decomposition (ie implementing StateModifier) containing all changes to be made to a search state.
        Decomposition dec = Factory.getDecomposition(st.pb, act, absDec);

        // remember that the consuming db has to be supporting by a descendant of this decomposition.
        st.addSupportConstraint(consumer.GetChainComponent(0), dec);

        st.applyDecomposition(dec);

        return true;
    }
}
