package fr.laas.fape.planning.core.planning.tasknetworks;

import fr.laas.fape.anml.model.concrete.statements.LogStatement;
import fr.laas.fape.graph.GraphFactory;
import fr.laas.fape.graph.core.Edge;
import fr.laas.fape.graph.core.SimpleUnlabeledDigraph;
import fr.laas.fape.graph.core.UnlabeledDigraph;
import fr.laas.fape.graph.printers.NodeEdgePrinter;
import fr.laas.fape.planning.core.planning.states.Printer;
import fr.laas.fape.planning.core.planning.states.PartialPlan;
import fr.laas.fape.planning.exceptions.FAPEException;
import fr.laas.fape.anml.model.concrete.Action;
import fr.laas.fape.anml.model.concrete.Task;
import fr.laas.fape.planning.util.Reporter;
import fr.laas.fape.structures.Pair;


import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class TaskNetworkManager implements Reporter {

    private final UnlabeledDigraph<TNNode> network;

    private int numRoots;
    private int numOpenTasks;
    private int numUnmotivatedActions;
    private final List<Action> actions;


    public TaskNetworkManager() {
        network = GraphFactory.getSimpleUnlabeledDigraph();
        numRoots = 0;
        numOpenTasks = 0;
        numUnmotivatedActions = 0;
        actions = new ArrayList<>();
    }

    private TaskNetworkManager(TaskNetworkManager base) {
        this.network = base.network.cc();
        this.numRoots = base.numRoots;
        this.numOpenTasks = base.numOpenTasks;
        this.numUnmotivatedActions = base.numUnmotivatedActions;
        actions = new ArrayList<>(base.actions);
    }

    private List<Task> openTasks = null;

    private void clearCache() {
        openTasks = null;
    }

    /**
     * O(1)
     * @return The number of actions in the task network (all actions are considered).
     */
    public int getNumActions() {
        return actions.size();
    }

    /**
     * O(1)
     * @return Number of roots in the task network.
     */
    public int getNumRoots() {
        return numRoots;
    }

    /**
     * O(1)
     * @return Number of opened action conditions.
     */
    public int getNumOpenTasks() { return numOpenTasks; }

    /**
     * O(1)
     * @return Number of action with motivated keyword that are motivated yet.
     */
    public int getNumUnmotivatedActions() { return numUnmotivatedActions; }

    /**
     * O(1)
     * @param a The action to check
     * @return True if the action is a root of the task network (e.g. it is not part
     *         of any decomposition.
     */
    public boolean isRoot(TNNode n) {
        return network.inDegree(n) == 0;
    }

    /**
     * O(n)
     * @return All actions of the task network that are not issued from a decomposition
     *         (ie. roots of the task network).
     */
    public List<TNNode> roots() {
        List<TNNode> roots = new LinkedList<>();
        for(TNNode n : network.jVertices()) {
            if(isRoot(n)) {
                roots.add(n);
            }
        }
        assert roots.size() == numRoots : "Error: wrong number of roots. Collected " + roots.size() + " but expected " + numRoots + ".\n" +
                "Roots: " + roots.toString();
        return roots;
    }

    /**
     * @param task Task to lookup
     * @return True if the action condition is supported (i.e. there is an edge
     *         from task to an action.
     */
    public boolean isSupported(Task task) {
        for(TNNode child : network.jChildren(new TNNode(task))) {
            if(child.isAction()) {
                return true;
            }
        }
        return false;
    }

    private TNNode getParent(TNNode node) {
        if(network.inDegree(node) == 0)
            return null;
        else
            return network.jParents(node).stream().findFirst().get();
    }

    public Action getContainingAction(Action a) {
        TNNode father = getParent(new TNNode(a));
        if(father == null)
            return null;
        assert father.isTask();
        TNNode grandFather = getParent(father);
        if(grandFather == null)
            return null;
        assert grandFather.isAction();
        return grandFather.asAction();
    }

    public Task getRefinedTask(Action a) {
        TNNode father = getParent(new TNNode(a));
        if(father == null)
            return null;
        assert father.isTask();
        return father.asTask();
    }

    /**
     * O(n)
     * @return All action condition that are not supported yet.
     */
    public List<Task> getOpenTasks() {
        if(openTasks == null) {
            List<Task> l = new ArrayList<>();
            for (TNNode n : network.jVertices()) {
                if (n.isTask()) {
                    Task task = n.asTask();
                    if (!isSupported(task)) {
                        l.add(task);
                    }
                }
            }
            openTasks = l;
        }
        assert openTasks.size() == numOpenTasks;
        return openTasks;
    }

    public Collection<Task> getAllTasks() {
        List<Task> l = new LinkedList<>();
        for (TNNode n : network.jVertices()) {
            if(n.isTask()) {
                Task task = n.asTask();
                l.add(task);
            }
        }
        return l;
    }

    /**
     * O(n)
     * @return All actions that have a motivated statements but are not yet
     *         part of any decomposition.
     */
    public List<Action> getNonSupportedMotivatedActions() {
        LinkedList<Action> l = new LinkedList<>();
        for (TNNode n : network.jVertices()) {
            if(n.isAction()) {
                Action a = n.asAction();
                if(a.mustBeMotivated() && !isSupporting(a)) {
                    l.add(a);
                }
            }
        }
        assert l.size() == numUnmotivatedActions;
        return l;
    }

    /**
     * @return  True if this action supports a task
     */
    public boolean isSupporting(Action a) {
        TNNode n = new TNNode(a);
        return network.inDegree(n) > 0;
    }

    /**
     * add a task support link from a task to an action.
     * THis link means: the task t is supported by the action a.
     *
     * An task should be supported by exactly one action.
     * @param t A task already present in the task network.
     * @param a An action already present in the task network.
     */
    public void addSupport(Task t, Action a) {
        assert network.contains(new TNNode(t));
        assert network.contains(new TNNode(a));
        assert network.outDegree(new TNNode(t)) == 0;
        network.addEdge(new TNNode(t), new TNNode(a));
        if(network.inDegree(new TNNode(a)) == 1) {
            if (!a.hasParent()) {
                numRoots--;
            }
            if(a.mustBeMotivated())
                numUnmotivatedActions--;
        }
        numOpenTasks--;
        clearCache();
    }

    /**
     * Inserts an action in the task network. If the action a has
     * a father p, an edge from the decomposition of p to a is also added.
     * O(1)
     */
    public void insert(Action a) {
        network.addVertex(new TNNode(a));
        actions.add(a);

        if(a.hasParent()) {
            // network.addEdge(new TNNode(a.parent()), new TNNode(a));
        } else {
            numRoots++;
        }
        if(a.mustBeMotivated() && !isSupporting(a))
            numUnmotivatedActions++;
        clearCache();
    }

    /**
     * Adds an action condition to an action.
     * @param task The action condition.
     * @param parent The action in which task appears. This action must be already
     *               present in the task network.
     */
    public void insert(Task task, Action parent) {
        network.addVertex(new TNNode(task));
        network.addEdge(new TNNode(parent), new TNNode(task));
        task.setParent(parent);
        numOpenTasks++;
        clearCache();
    }

    public void setSubtaskOf(Task t, Action parent) {
        network.addEdge(new TNNode(parent), new TNNode(t));
        if (!t.hasParent()) {
            numRoots--;
        }
        t.setParent(parent);
        clearCache();
    }

    /**
     * Adds task in the task network.
     * @param task The task to add.
     */
    public void insert(Task task) {
        network.addVertex(new TNNode(task));
        numOpenTasks++;
        if (!task.hasParent()) {
            numRoots++;
        }
        clearCache();
    }

    public boolean isDescendantOf(Action child, TNNode potentialAncestor) {
        return isDescendantOf(new TNNode(child), potentialAncestor);
    }

    public boolean isDescendantOf(Task child, TNNode potentialAncestor) {
        return isDescendantOf(new TNNode(child), potentialAncestor);
    }

    public boolean isDescendantOf(Action child, Task potentialAncestor) {
        return isDescendantOf(new TNNode(child), new TNNode(potentialAncestor));
    }

    public boolean isDescendantOf(Task child, Task potentialAncestor) {
        return isDescendantOf(new TNNode(child), new TNNode(potentialAncestor));
    }

    public boolean isDescendantOf(Action child, Action potentialAncestor) {
        return isDescendantOf(new TNNode(child), new TNNode(potentialAncestor));
    }

    public boolean isDescendantOf(Task child, Action potentialAncestor) {
        return isDescendantOf(new TNNode(child), new TNNode(potentialAncestor));
    }

    /**
     * Checks if the first node is a descendant of the second one.
     * (i.e. there is a path from n2 to n1).
     * @param n1 Descendant node.
     * @param n2 Potential ancestor.
     * @return True if n2 is an ancestor of n1.
     */
    private boolean isDescendantOf(TNNode n1, TNNode n2) {
        if(network.inDegree(n1) == 0) {
            return false;
        } else if(network.parents(n1).size() != 1) {
            throw new FAPEException("Error: node "+n1+" has more than one father: " + network.parents(n1).toString());
        } else if(network.parents(n1).contains(n2)) {
            return true;
        } else {
            return isDescendantOf(network.parents(n1).head(), n2);
        }
    }

    public Pair<Action, Integer> leastCommonAncestor(Action a1, Action a2) {
        List<Action> parents = new LinkedList<>();
        TNNode cur = new TNNode(a1);
        parents.add(a1);
        while(cur != null) {
            if(network.parents(cur).nonEmpty()) {
                assert network.parents(cur).size() == 1;
                cur = network.parents(cur).head();
                if(cur.isAction())
                    parents.add(cur.asAction());
            } else {
                cur = null;
            }
        }

        cur = new TNNode(a2);
        Action commonAncestor = null;
        int depth = -1;
        while(cur != null && commonAncestor == null) {
            if(cur.isAction()) {
                depth += 1;
                if(parents.contains(cur.asAction())) {
                    commonAncestor = cur.asAction();
                    break;
                }
            }
            if(network.parents(cur).nonEmpty()) {
                assert network.parents(cur).size() == 1;
                cur = network.parents(cur).head();
            } else {
                cur = null;
            }
        }

        if(commonAncestor != null)
            return new Pair<>(commonAncestor, depth);
        else
            return null;
    }

    public List<Action> children(Task t) {
        return network.jChildren(new TNNode(t)).stream().map(TNNode::asAction).collect(Collectors.toList());
    }

    public List<Task> children(Action a) {
        return network.jChildren(new TNNode(a)).stream().map(TNNode::asTask).collect(Collectors.toList());
    }

    /**
     * Performs recursively a deep copy on the task network manager and all its
     * actions.
     * @return A new TaskManager with the same content.
     */
    public TaskNetworkManager deepCopy() {
        return new TaskNetworkManager(this);
    }

    @Override
    public String report() {
        String str = "Num roots: " + roots().size() + ", roots: " + roots().toString();
        str += "\n\tLeaf actions" +  getAllActions().toString();
        return str;
    }

    /**
     * O(1).
     * @return All actions of the task network.
     */
    public List<Action> getAllActions() {
        return actions;
    }

    /**
     * Lookup the action containing the given statement.
     * Implementations currently looks for all statements of all actions
     * O(n)
     * TODO: more efficient implementation
     * TODO: account for decompositions?
     *
     * @param e LogStatement to look for.
     * @return The action containing the statement. null if no action in the task network contains the statement.
     */
    public Action getActionContainingStatement(LogStatement e) {
        for(Action a : getAllActions()) {
            for(LogStatement s : a.logStatements()) {
                if(s.equals(e)) {
                    return a;
                }
            }
        }
        return null;
    }



    public void exportToDot(final PartialPlan st, String filename) {
        class TNPrinter extends NodeEdgePrinter<TNNode, Object, Edge<TNNode>> {
            @Override
            public String printNode(TNNode n) {
                assert n.isAction();
                return Printer.action(st, n.asAction());
            }
        }

        SimpleUnlabeledDigraph<TNNode> g = GraphFactory.getSimpleUnlabeledDigraph();
        for(TNNode n : network.jVertices())
            if(n.isAction())
                g.addVertex(n);

        for(TNNode n : network.jVertices()) {
            if (n.isAction()) {
                for(TNNode child : network.jChildren(n)) {
                    if(child.isAction()) {
                        g.addEdge(n, child);
                    } else {
                        for(TNNode grandChild : network.jChildren(child)) {
                            if(grandChild.isAction()) {
                                g.addEdge(n, grandChild);
                            } else {
                                for(TNNode grandGrandChild : network.jChildren(grandChild)) {
                                    assert grandGrandChild.isAction();
                                    g.addEdge(n, grandGrandChild);
                                }
                            }
                        }
                    }
                }
            }
        }
        g.exportToDotFile(filename, new TNPrinter());
    }

    public boolean contains(Action a) {
        return network.contains(new TNNode(a));
    }

    public boolean contains(Task t) {
        return network.contains(new TNNode(t));
    }
}
