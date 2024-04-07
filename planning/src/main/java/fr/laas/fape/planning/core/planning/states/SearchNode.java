package fr.laas.fape.planning.core.planning.states;

import fr.laas.fape.planning.core.planning.planner.Planner;
import fr.laas.fape.planning.util.StrongReference;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * Node of the search tree.
 *
 * A node is a description to build a State :
 *  - a link to the preceding node in the search tree from which a base state can be obtained
 *  - a list of operation to apply to the base state.
 *
 * The State object is cached using soft references. If the JVM runs out of memory, cached states
 * are reclaimed by the garbage collector. The least recently used state is typically reclaimed first.
 *
 * The search node also provides a caching mechanism for heuristic values
 * (to avoid rebuilding a complete state to extract the heuristic value).
 */
public class SearchNode {

    public enum Status {
        STABLE, // DO not clean
        PENDING, // not yet expanded
        EXPANDED, // was expanded
        INCONSISTENT // was expanded and found to be inconsistent
    }

    public SearchNode(SearchNode parent, Planner planner) {
        this.parent = parent;
        state = null;
        this.mID = PartialPlan.idCounter++;
        this.depth = parent.depth +1;
        planner.recordSearchNode(this);
        status = Status.PENDING;
    }
    public SearchNode(PartialPlan initialPartialPlan, Planner planner) {
        this.parent = null;
        state = new StrongReference<>(initialPartialPlan);
        this.mID = initialPartialPlan.mID;
        this.depth = 0;
        planner.recordSearchNode(this);
        lastRecord = System.nanoTime();
        status = Status.STABLE;
    }

    /** Identifier of the contained state. */
    final int mID;

    /** Depth of this search node */
    final int depth;

    /** A soft, strong or weak reference to a state. */
    private Reference<PartialPlan> state = null;

    /** Predecessor in hte search tree */
    private final SearchNode parent;

    /** Operations to apply to the parent's state to get a complete state. */
    private List<Consumer<PartialPlan>> operations = new ArrayList<>();

    /**
     * Index of the the next operation to apply to state (all operations below this
     * index were already applied
     */
    private int nextOperation = 0;
    public Status status;

    /** Nano-time at which a PartialPlan (if any) was recorded in this node. */
    public long lastRecord = 0;

    public boolean holdsMemory() {
        return state != null &&
                (state instanceof SoftReference || state instanceof StrongReference) &&
                state.get() != null;
    }

    public void setExpanded() {
        if(status == Status.PENDING)
            status = Status.EXPANDED;
        PartialPlan s = state.get();
        if(status != Status.STABLE)
            releaseMemory();
    }

    public void setInconsistent() {
        status = Status.INCONSISTENT;
    }

    public void releaseMemory() {
        assert status != Status.STABLE;
        if(state != null) {
            state = new WeakReference<>(state.get());
            lastRecord = 0;
        }
    }

    /** List of 'h', 'g', 'hc' heuristic values */
    private double h = -1;
    private double g = -1;
    private double hc = -1;

    public double getH() { return h; }
    public boolean isRecordedH() { return h >= 0; }
    public void setH(double value) { h = value; }

    public double getG() { return g; }
    public boolean isRecordedG() { return g >= 0; }
    public void setG(double value) { g = value; }

    public double getHC() { return hc; }
    public boolean isRecordedHC() { return hc >= 0; }
    public void setHC(double value) { hc = value; }

    /**
     * Returns the base state from which the complete state can be built.
     */
    private PartialPlan getBaseState(boolean isForChild) {
        if(state != null && state.get() != null) {
            return state.get();
        } else {
            assert depth != 0;
            assert parent != null;
            PartialPlan st = parent.getState(true).cc(mID);
            st.depth = depth;
            nextOperation = 0;
            if(!isForChild) {// directly request, save the reference
                state = new SoftReference<>(st);
                lastRecord = System.nanoTime();
            } else if(depth %5 == 0) { // save at regular depths
                state = new SoftReference<>(st);
                lastRecord = System.nanoTime();
            }

            return st;
        }
    }

    public SearchNode getParent() { return parent; }

    public PartialPlan getState() {
        return getState(false);
    }

    private PartialPlan getState(boolean isForChild) {
        PartialPlan s = getBaseState(isForChild);
        s.depth = depth;
        while(nextOperation < operations.size()) {
            operations.get(nextOperation++).accept(s);
        }
        return s;
    }

    public int getDepth() { return depth; }

    /**
     * Appends a new operation necessary to build the complete state.
     */
    public void addOperation(Consumer<PartialPlan> operation) {
        operations.add(operation);
    }

    public int getID() {
        return this.mID;
    }

    /** This method give a false assert if the it is not called from either:
     *  - a getState call, meaning that is part of a recorded operation
     *  - a State.update, meaning that is part of the initialization of the first state.
     *  Any method that modifies a state should verify that */
    public static void assertPartOfRecordeedOperation() {
        if(false) {
            StackTraceElement[] elems = Thread.currentThread().getStackTrace();
            for (StackTraceElement e : elems)
                if (e.getClassName().equals("SearchNode") && e.getMethodName().equals("getState")
                        || (e.getClassName().equals("State") && e.getMethodName().equals("update")))
                    return;

            assert false;
        }
    }

    @Override
    public String toString() {
        return "SearchNode{" +
                "mID=" + mID +
                ", depth=" + depth +
                ", status=" + status +
                '}';
    }
}
