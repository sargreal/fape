package fape.core.planning.heuristics.relaxed;

import fape.core.planning.grounding.DisjunctiveFluent;
import fape.core.planning.grounding.Fluent;
import fape.core.planning.grounding.GAction;
import fape.core.planning.planner.APlanner;
import fape.core.planning.planninggraph.FeasibilityReasoner;
import fape.core.planning.states.State;
import fape.core.planning.timelines.ChainComponent;
import fape.core.planning.timelines.Timeline;
import planstack.anml.model.LStatementRef;
import planstack.anml.model.concrete.Action;
import planstack.anml.model.concrete.InstanceRef;
import planstack.anml.model.concrete.TPRef;
import planstack.anml.model.concrete.statements.Assignment;
import planstack.anml.model.concrete.statements.LogStatement;

import java.util.Arrays;
import java.util.Collection;
import java.util.PrimitiveIterator;

public class DTGImpl {

    private static int EDGE_SIZE = 4; /* src, dest, lifted act, ground act */
    private static int NODE_SIZE = 6; /* fluent, level, start, end */
    private static int EMPTY_NODE = -5;

    private static int ACCEPTING = 1, NON_ACCEPTING = 0;
    private static int ENTRY_POINT =1, NOT_ENTRY_POINT = 0;

    private int nextNode = 1;
    private int nextEdge = 0;

    private int edgeIndex(int edge) { return edge * EDGE_SIZE; }
    private int nodeIndex(int node) { return node * NODE_SIZE; }
    private int id(int fluent, int lvl) { return fluentByLvl[(fluent * numLevels) + lvl]; }
    private int id(Fluent f, int lvl) { if(f != null) return id(f.ID, lvl); else return 0; }
    private boolean hasNode(int fluent, int lvl) { return (fluent*numLevels) < fluentByLvl.length && id(fluent, lvl) != -1; }
    private void setId(int id, int fluent, int lvl) {
        if(fluentByLvl.length <= fluent*numLevels) {
            final int oldSize = fluentByLvl.length;
            fluentByLvl = Arrays.copyOf(fluentByLvl, (fluent+1)*numLevels);
            Arrays.fill(fluentByLvl, oldSize, fluentByLvl.length, -1);
        }
        fluentByLvl[(fluent * numLevels)+lvl] = id; }

    private int source(int edge) { return edges[edgeIndex(edge)]; }
    private int dest(int edge) { return edges[edgeIndex(edge)+1]; }
    private int laction(int edge) { return edges[edgeIndex(edge)+2]; }
    private int gaction(int edge) { return edges[edgeIndex(edge)+3]; }

    private void setEdge(int edge, int source, int dest, int lifted, int ground) {
        assert source != -1 && dest != -1;
        final int idx = edgeIndex(edge);
        edges[idx] = source;
        edges[idx+1] = dest;
        edges[idx+2] = lifted;
        edges[idx+3] = ground;

        if(numInEdge(dest) >= inEdges[dest].length-1)
            inEdges[dest] = Arrays.copyOf(inEdges[dest], inEdges[dest].length*2);
        inEdges[dest][0] += 1; // increment number
        inEdges[dest][inEdges[dest][0]] = edge;
        // TODO in out
    }

    private int fluent(int node) { return nodes[nodeIndex(node)]; }
    private int lvl(int node) { return nodes[nodeIndex(node)+1]; }
    private int start(int node) { return nodes[nodeIndex(node)+2]; }
    private int end(int node) { return nodes[nodeIndex(node)+3]; }
    private boolean accepting(int node) { return nodes[nodeIndex(node)+4] == ACCEPTING; }
    private boolean entryPoint(int node) { return nodes[nodeIndex(node)+5] == ENTRY_POINT; }
    private int numInEdge(int node) { return inEdges[node][0]; }

    private void setAccepting(int node) { nodes[nodeIndex(node)+4] = ACCEPTING; }
    private void setEntryPoint(int node) { nodes[nodeIndex(node)+5] = ENTRY_POINT; }


    private void setNode(int node, int fluent, int lvl, int start, int end) {
        final int idx =nodeIndex(node);
        nodes[idx] = fluent;
        nodes[idx+1] = lvl;
        nodes[idx+2] = start;
        nodes[idx+3] = end;
        inEdges[node] = new int[5];
        inEdges[node][0] = 0; // first one is the number of edges in this list
        setId(node, fluent, lvl);
    }

    private int nextEdgeID() {
        if(edges.length <= nextEdge * EDGE_SIZE) // grow
            edges = Arrays.copyOf(edges, edges.length*2); //new int[tmp.length*2];
        return nextEdge++;
    }
    private int nextNodeID() {
        if(nodes.length <= nextNode * NODE_SIZE) {
            nodes = Arrays.copyOf(nodes, nodes.length*2);
            inEdges = Arrays.copyOf(inEdges, inEdges.length*2);
        }
        return nextNode++;
    }

    public int addNode(Fluent f, int lvl, TPRef start, TPRef end) {
        if(f == null) {
            assert lvl == 0 : "Null node can only exist at level 0.";
            return 0; // null fluent is always represented by node 0.
        }
        assert !hasNode(f.ID, lvl) : "Node already recorded.";
        int id = nextNodeID();
        setNode(id, f.ID, lvl, start != null ? start.id() : -1, end != null ?end.id() : -1);
        return id;
    }

    public int addEdge(Fluent src, int srcLvl, Action lifted, GAction ground, Fluent dest, int destLvl) {
        assert srcLvl < numLevels && destLvl < numLevels;
        assert hasNode(src, srcLvl) : "Source node not recorded.";
        assert hasNode(dest, destLvl) : "Dest node not recorded.";
        int id = nextEdgeID();
        setEdge(id, id(src, srcLvl), id(dest, destLvl), lifted!=null ? lifted.id().id() : -1, ground != null ? ground.id :-1);
        return id;
    }

    int[] edges = new int[10*EDGE_SIZE];
    int[] nodes = new int[10*NODE_SIZE];
    final int numLevels;
    int[] fluentByLvl;
    int[][] inEdges = new int[10][];

    public DTGImpl(int numLevels) {
        this.numLevels = numLevels;
        fluentByLvl = new int[10*numLevels];
        Arrays.fill(fluentByLvl, -1);
    }

    public PrimitiveIterator.OfInt inEdgesIterator(final int node) {
        return new PrimitiveIterator.OfInt() {
            final int[] edges = inEdges[node];
            int current = 0;
            @Override public int nextInt() {
                assert edges[current+1] != -1;
                return edges[1+current++];
            }

            @Override
            public boolean hasNext() {
                return current < edges[0];
            }
        };
    }

    public void setAccepting(Fluent f, int lvl) {
        setAccepting(id(f, lvl));
    }

    public void setEntryPoint(Fluent f, int lvl) {
        setEntryPoint(id(f, lvl));
    }

    public boolean hasNode(Fluent f, int level) {
        if(f != null)
            return hasNode(f.ID, level);
        assert level == 0;
        return true;
    }




    public static DTGImpl buildFromTimeline(Timeline tl, APlanner planner, State st) {
        DTGImpl dtg = new DTGImpl(tl.numChanges()+1);

        FeasibilityReasoner reas = planner.preprocessor.getFeasibilityReasoner();

        for(int i=0 ; i<tl.numChanges() ; i++) {
            ChainComponent cc = tl.getChangeNumber(i);

            LogStatement s = cc.getFirst();
            if(i == 0) { // first statement
                if(s instanceof Assignment) {
                    dtg.addNode(null, 0, null, s.start());
                    dtg.setAccepting(null, 0);
                } else {
                    Collection<Fluent> fluents = DisjunctiveFluent.fluentsOf(s.sv(), s.startValue(), st, planner);
                    for(Fluent f : fluents) {
                        dtg.addNode(f, 0, null, s.start());
                        dtg.setAccepting(f, 0);
                    }
                }
            }

            // action by which this statement was introduced (null if no action)
            Action containingAction = st.getActionContaining(s);
            TPRef start = s.end();
            TPRef end = i+1 < tl.numChanges() ? tl.getChangeNumber(i+1).getConsumeTimePoint() : null;

            if(containingAction == null) { // statement was not added as part of an action
                assert s instanceof Assignment;
                assert s.endValue() instanceof InstanceRef;
                assert i == 0;
                Collection<Fluent> fluents = DisjunctiveFluent.fluentsOf(s.sv(), s.endValue(), st, planner); //todo why false

                for(Fluent f : fluents) {
                    assert(dtg.hasNode(null, i)); // FROM
                    if(!dtg.hasNode(f, i + 1)) // TO
                        dtg.addNode(f, i + 1, start, end);
                    dtg.addEdge(null, i, null, null, f, i+1);

                    if(i == tl.numChanges() -1) { // this is the last transition of this timeline, link to the DTG
                        dtg.setEntryPoint(f, i + 1);
                    }
                }
            } else { // statement was added as part of an action or a decomposition
                Collection<GAction> acts = reas.getGroundActions(containingAction, st);

                // local reference of the statement, used to extract the corresponding ground statement from the GAction
                LStatementRef statementRef;
                if(containingAction.context().contains(s)) {
                    // statement defined in the action
                    statementRef = containingAction.context().getRefOfStatement(s);
                } else {
                    // statement not defined in the action, it is necesarily defined in the decomposition of this action
                    assert st.taskNet.isDecomposed(containingAction) && st.taskNet.getDecomposition(containingAction).context().contains(s);
                    statementRef = st.taskNet.getDecomposition(containingAction).context().getRefOfStatement(s);
                }

                for(GAction ga : acts) {
                    GAction.GLogStatement gs = ga.statementWithRef(statementRef);
                    Fluent fromFluent;
                    Fluent toFluent;

                    if(gs instanceof GAction.GTransition) {
                        fromFluent = planner.preprocessor.getFluent(gs.sv, ((GAction.GTransition) gs).from);
                        toFluent = planner.preprocessor.getFluent(gs.sv, ((GAction.GTransition) gs).to);

                    } else {
                        assert gs instanceof GAction.GAssignement;
                        fromFluent = null;
                        toFluent = planner.preprocessor.getFluent(gs.sv, ((GAction.GAssignement) gs).to);
                    }

                    if(dtg.hasNode(fromFluent, i)) {
                        if(!dtg.hasNode(toFluent, i+1))
                            dtg.addNode(toFluent, i+1, start, end);

                        dtg.addEdge(fromFluent, i, containingAction, ga, toFluent, i+1);
                        if(i == tl.numChanges()-1) {
                            dtg.setEntryPoint(toFluent, i+1);
                        }
                    }
                }
            }
        }
        return dtg;
    }
}
