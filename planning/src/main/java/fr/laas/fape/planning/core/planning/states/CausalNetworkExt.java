package fr.laas.fape.planning.core.planning.states;

import fr.laas.fape.anml.model.concrete.TPRef;
import fr.laas.fape.anml.model.concrete.statements.LogStatement;
import fr.laas.fape.planning.core.planning.planner.Counters;
import fr.laas.fape.planning.core.planning.planner.GlobalOptions;
import fr.laas.fape.planning.core.planning.timelines.ChainComponent;
import fr.laas.fape.exceptions.InconsistencyException;
import fr.laas.fape.planning.core.planning.timelines.Timeline;
import fr.laas.fape.planning.core.planning.timelines.TimelinesManager;
import fr.laas.fape.structures.ISet;
import lombok.EqualsAndHashCode;
import lombok.Value;
import fr.laas.fape.anml.model.ParameterizedStateVariable;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class CausalNetworkExt implements StateExtension {
    // for evaluation purposes, setting it two false will only keep the most basic evaluation modes
    public final boolean USE_CAUSAL_NETWORK = GlobalOptions.getBooleanOption("use-causal-network");

    @Value @EqualsAndHashCode(callSuper = false)
    private class NoCompatibleTimelineForOpenGoal extends InconsistencyException {
        final Timeline tl;
    }

    @Value
    public static class Event {
        public final int supporterID;
        public final int changeNumber;
        public final LogStatement statement;
        public final int consumerID;
    }
    private Timeline timelineOf(Event e) { return container.getTimeline(e.getSupporterID()); }
    private ChainComponent componentOf(Event e) { return timelineOf(e).getChangeNumber(e.getChangeNumber()); }

    private final PartialPlan container;

    // maps a timeline (by its ID) to a set of possibly indirectly supporting events
    private final HashMap<Integer, ISet<Event>> potentialSupporters;
    // maps a timeline ID to number of the last event processed
    private final HashMap<Integer,Integer> lastProcessedChange;
    // respectively keep track of the IDs of timelines extended, added or removed since the last update
    private final List<Integer> extendedTimelines;
    private final Set<Integer> addedTimelines;
    private final List<Integer> removedTimelines;

    private final Map<Event, BitSet> possiblyInterferingTimelines;

    CausalNetworkExt(PartialPlan container) {
        this.container = container;
        potentialSupporters = new HashMap<>();
        lastProcessedChange = new HashMap<>();
        extendedTimelines = new ArrayList<>();
        removedTimelines = new ArrayList<>();
        addedTimelines = new HashSet<>(container.tdb.getTimelinesStream().map(t -> t.mID).collect(Collectors.toList()));
        possiblyInterferingTimelines = new HashMap<>();
    }

    private CausalNetworkExt(CausalNetworkExt toCopy, PartialPlan container) {
        this.container = container;
        potentialSupporters = new HashMap<>(toCopy.potentialSupporters);
        lastProcessedChange = new HashMap<>(toCopy.lastProcessedChange);
        extendedTimelines = new ArrayList<>(toCopy.extendedTimelines);
        addedTimelines = new HashSet<>(toCopy.addedTimelines);
        removedTimelines = new ArrayList<>(toCopy.removedTimelines);
        possiblyInterferingTimelines = new HashMap<>();
        for(HashMap.Entry<Event,BitSet> e : toCopy.possiblyInterferingTimelines.entrySet()) {
            possiblyInterferingTimelines.put(e.getKey(), (BitSet) e.getValue().clone());
        }
    }

    @Override
    public StateExtension clone(PartialPlan st) {
        return new CausalNetworkExt(this, st);
    }

    public ISet<Event> getPotentialIndirectSupporters(Timeline tl) {
        processPending();
        return potentialSupporters.get(tl.mID);
    }

    ISet<Event> getPotentialSupporters(Timeline tl) {
        processPending();
        return potentialSupporters.get(tl.mID).filter((Predicate<Event>) e ->
                container.unifiable(
                        container.getTimeline(tl.mID).getGlobalConsumeValue(),
                        container.getTimeline(e.supporterID).getChangeNumber(e.changeNumber).getSupportValue()));
    }

    private void processPending() {
        Counters.inc("process-pending");
        if(extendedTimelines.isEmpty() && addedTimelines.isEmpty() && removedTimelines.isEmpty())
            return;
        Counters.inc("process-pending-non-noop");
        // find all indirect supporters of newly added timelines
        for(int tlID : addedTimelines) {
            if(!container.tdb.containsTimelineWithID(tlID))
                continue;
            Timeline tl = container.tdb.getTimeline(tlID);
            if(!tl.isConsumer())
                continue;
            assert !potentialSupporters.containsKey(tlID);
            potentialSupporters.put(tlID, new ISet<>());

            for(Timeline sup : container.tdb.getTimelines()) {
                for(int i=0 ; i<sup.numChanges() ; i++) {
                    if (mightIndirectlySupport(sup, i, tl)) {
                        LogStatement ls = sup.getChangeNumber(i).getFirst();
                        Event pis = new Event(sup.mID, i, ls, tlID);
                        potentialSupporters.put(tlID, potentialSupporters.get(tlID).with(pis));
                    }
                }
            }
        }

        TimelinesManager tlMan = container.tdb;


        for(int tlID : potentialSupporters.keySet()) {
            if(!container.tdb.containsTimelineWithID(tlID)) {
                assert removedTimelines.contains(tlID);
                // timeline was deleted, remove any reference we might have
                potentialSupporters.remove(tlID);
                continue;
            }
            Timeline tl = tlMan.getTimeline(tlID);

            // incrementaly update timelines that were not added (new timelines were previously processed already)
            if(!addedTimelines.contains(tlID)) {
                // the timeline was previously there, process the updated timelines to see if there are new potential supporters
                addedTimelines.stream()
                        .filter(tlMan::containsTimelineWithID)
                        .map(tlMan::getTimeline)
                        .forEach(sup -> {
                            for (int i = 0; i < sup.numChanges(); i++) {
                                if (mightIndirectlySupport(sup, i, tl)) {
                                    LogStatement ls = sup.getChangeNumber(i).getFirst();
                                    Event pis = new Event(sup.mID, i, ls, tlID);
                                    potentialSupporters.put(tlID, potentialSupporters.get(tlID).with(pis));
                                }
                            }
                        });
                extendedTimelines.stream()
                        .filter(id -> !addedTimelines.contains(id))
                        .filter(tlMan::containsTimelineWithID)
                        .map(tlMan::getTimeline)
                        .forEach(sup -> {
                            assert lastProcessedChange.containsKey(sup.mID);
                            for (int i = lastProcessedChange.get(sup.mID)+1; i < sup.numChanges(); i++) {
                                if (mightIndirectlySupport(sup, i, tl)) {
                                    LogStatement ls = sup.getChangeNumber(i).getFirst();
                                    Event pis = new Event(sup.mID, i, ls, tlID);
                                    potentialSupporters.put(tlID, potentialSupporters.get(tlID).with(pis));
                                }
                            }
                        });
                if(!removedTimelines.isEmpty()) {
                    Set<Integer> supporters = potentialSupporters.get(tlID).stream().map(e -> e.supporterID).collect(Collectors.toSet());
                    if(removedTimelines.stream().anyMatch(id -> supporters.contains(id))) {
                        ISet<Event> updatedSups = potentialSupporters.get(tlID).filter((Predicate<Event>) e -> !removedTimelines.contains(e.supporterID));
                        potentialSupporters.put(tlID, updatedSups);
                    }
                }
            }
        }
        filterUnfeasibleSupports();

        addedTimelines.clear();
        extendedTimelines.clear();
        removedTimelines.clear();
        lastProcessedChange.clear();
        for(Timeline tl : tlMan.getTimelines()) {
            lastProcessedChange.put(tl.mID, tl.numChanges()-1);
        }
    }

    private void filterUnfeasibleSupports() {
        TimelinesManager tlMan = container.tdb;

        for(int tlID : potentialSupporters.keySet()) {
            Timeline tl = tlMan.getTimeline(tlID);

            Set<Event> toRemove = new HashSet<>();
            for(Event pis : potentialSupporters.get(tlID)) {
                assert tlID == pis.consumerID;
                // if this event can not be separated (temporally or logically) from the open goal,
                // then it is the only possible supporter
                if(USE_CAUSAL_NETWORK) {
                    if (!canBeSeparated(pis, tlMan.getTimeline(tlID))) {
                        toRemove.addAll(potentialSupporters.get(tlID).filter((Predicate<Event>) (e -> e != pis)).asJava());
                    }
                }

                Timeline sup = tlMan.getTimeline(pis.supporterID);
                if(!mightIndirectlySupport(sup, pis.changeNumber, tl)) {
                    toRemove.add(pis);
                    continue;
                }

                // when possible infer temporal constraints on the earliest appearance of the open goal
                if(USE_CAUSAL_NETWORK && container.pl.options.checkUnsolvableThreatsForOpenGoalsResolvers) {

                    // get the initial list
                    if(!possiblyInterferingTimelines.containsKey(pis)) {
                        // no incremental result to use, compute from scratch
                        BitSet list = new BitSet();
                        possiblyInterferingTimelines.put(pis, list);
                        for(Timeline threat : tlMan.getTimelines()) {
                            list.set(threat.mID);
                        }
                    } else {
                        // process incrementally
                        BitSet list  = possiblyInterferingTimelines.get(pis);
                        for(int i : addedTimelines) {
                            list.set(i);
                        }
                    }

                    if(isInfeasibleDueToInterferences(pis))
                        toRemove.add(pis);
                }
            }

            for(Event pis : toRemove) {
                possiblyInterferingTimelines.remove(pis);
            }
            // System.out.println("Potential Supporters " + potentialSupporters.get(tlID));
            // System.out.println("Potential Supporters " + toRemove);
            potentialSupporters.put(tlID, potentialSupporters.get(tlID).withoutAll(toRemove));

            // infer any possible temporal constraints from the need for indirect supporters
            if(!container.pl.preprocessor.getHierarchicalEffects().hasAssignmentsInAction(tl.stateVariable.func())) {
                // open goal necessarily has an indirect supporter
                if(potentialSupporters.get(tlID).isEmpty()) {
                    throw new NoCompatibleTimelineForOpenGoal(tl);
                } else if (potentialSupporters.get(tlID).size() <= 1) {
                    Event e = potentialSupporters.get(tlID).head();
                    List<TPRef> timepoints = tl.hasSinglePersistence() ?
                            componentOf(e).getEndTimepoints() : timelineOf(e).timepointsPrecedingNextChange(componentOf(e));
                    container.enforceBefore(timepoints, tl.getConsumeTimePoint());
                } else {
                    if(tl.hasSinglePersistence()) {
                        int earliest = Integer.MAX_VALUE;
                        for(Event event : potentialSupporters.get(tlID)) {
                            int a = container.getMaxEarliestStartTime(componentOf(event).getEndTimepoints());
                            if(earliest > a)
                                earliest = a;
                        }
                        if(earliest != Integer.MAX_VALUE)
                            container.enforceDelay(container.pb.start(), tl.getConsumeTimePoint(), earliest);
                    } else {
                        int earliest = Integer.MAX_VALUE;
                        for(Event event : potentialSupporters.get(tlID)) {
                            int a = container.getMaxEarliestStartTime(timelineOf(event).timepointsPrecedingNextChange(componentOf(event)));
                            if(earliest > a)
                                earliest = a;
                        }
                        if(earliest != Integer.MAX_VALUE)
                            container.enforceDelay(container.pb.start(), tl.getConsumeTimePoint(), earliest);
                    }
                }
            }
        }
    }

    /** Filters the set of possibly interfering timelines for this support */
    private boolean isInfeasibleDueToInterferences(Event pis) {
        TimelinesManager tlMan = container.tdb;
        Timeline sup = tlMan.getTimeline(pis.supporterID);
        Timeline tl = tlMan.getTimeline(pis.consumerID);

        // mutable reference to the list of possibly interfering timelines
        BitSet list = possiblyInterferingTimelines.get(pis);
        assert list != null;

        // filter possible interference that are no longer threatening.
        int cur = list.nextSetBit(0);
        while(cur >= 0) {
            int i = cur;
            if(cur == pis.consumerID || cur == pis.supporterID)
                list.clear(cur);
            else if(!tlMan.containsTimelineWithID(i) || !possiblyInterfering(sup, tl, tlMan.getTimeline(i))) {
                list.clear(cur);
            }

            cur = list.nextSetBit(cur+1);
        }
        // look for necessary threats
        cur = list.nextSetBit(0);
        while(cur >= 0) {
            if (necessarilyThreatening(sup, tl, tlMan.getTimeline(cur))) {
                return true;
            }
            cur = list.nextSetBit(cur+1);
        }
        return false;
    }

    /** Returns true if either (i) there can be a temporal gap between the event and the timeline;
     * ro (ii) they can be on different fluents */
    private boolean canBeSeparated(Event e, Timeline og) {
        Timeline tl = container.getTimeline(e.supporterID);

        if(!areNecessarilyIdentical(e.getStatement().sv(), tl.stateVariable))
            return true;

        List<TPRef> endTimepoints;
        if(og.hasSinglePersistence()) {
            endTimepoints = Collections.singletonList(e.getStatement().end());
        } else {
            ChainComponent eventComp = tl.getChangeNumber(e.getChangeNumber());
            endTimepoints = tl.timepointsPrecedingNextChange(eventComp);
        }

        for(TPRef tp : endTimepoints) {
            boolean x = container.csp.stn().isDelayPossible(tp, og.getConsumeTimePoint(), 1);
            boolean y = container.csp.stn().isDelayPossible(og.getConsumeTimePoint(), tp, 1);
            if(!x && !y)
                return false;
        }
        return true;
    }

    /**
     * Returns true if the two state variables are necessarily identical.
     * This is true if they are on the same state variable and all their arguments are equals.
     */
    private boolean areNecessarilyIdentical(ParameterizedStateVariable sv1, ParameterizedStateVariable sv2) {
        if(sv1.func() != sv2.func())
            return false;
        for(int i=0 ; i<sv1.args().length ; i++) {
            if(container.separable(sv1.arg(i), sv2.arg(i)))
                return false;
        }
        return true;
    }

    private boolean possiblyInterfering(Timeline supporter, Timeline consumer, Timeline threat) {
        if(threat.hasSinglePersistence())
            return possiblyIntermediateStep(supporter, consumer, threat);
        else
            return possiblyThreatening(supporter, consumer, threat);
    }

    private boolean possiblyIntermediateStep(Timeline supporter, Timeline consumer, Timeline threat) {
        if(!threat.hasSinglePersistence())
            return false;
        if(!container.unifiable(supporter, threat))
            return false;
        if(!container.unifiable(consumer, threat))
            return false;
        if(!container.canAllBeBefore(supporter.getSupportTimePoint(), threat.getFirstTimePoints()))
            return false;
        if(!container.canAllBeBefore(threat.getLastTimePoints(), consumer.getFirstTimePoints()))
            return false;
        return true;
    }

    private boolean possiblyThreatening(Timeline supporter, Timeline consumer, Timeline threat) {
        if(threat.hasSinglePersistence())
            return false;
        if(!container.unifiable(supporter, threat))
            return false;
        if(!container.unifiable(consumer, threat))
            return false;
        if(!container.canAllBeBefore(supporter.getFirstChangeTimePoint(), threat.getLastTimePoints()))
            return false;
        if(!container.canAllBeBefore(threat.getFirstChangeTimePoint(), consumer.getLastTimePoints()))
            return false;
        return true;
    }

    private boolean necessarilyThreatening(Timeline supporter, Timeline consumer, Timeline threat) {
        if(!USE_CAUSAL_NETWORK)
            return false;

        if(threat.hasSinglePersistence())
            return false;
        if(!(container.unified(supporter, threat) || container.unified(consumer, threat)))
            return false;

        // check if [start(supporter),end(consumer)] must overlap [start(threat),end(threat)]

        boolean left = !container.canAllBeBefore(threat.getLastTimePoints(), supporter.getFirstChangeTimePoint());
        boolean right = !container.canAllBeBefore(consumer.getLastTimePoints(), threat.getFirstChangeTimePoint());
        return left && right;
    }

    private boolean necessarilyIntermediateStep(Timeline supporter, Timeline consumer, Timeline threat) {
        if (!threat.hasSinglePersistence())
            return false;
        if (!(container.unified(supporter, threat) || container.unified(consumer, threat)))
            return false;

        // check if [start(supporter),end(consumer)] must overlap [start(threat),end(threat)]
        boolean left = !container.canAllBeBefore(threat.getLastTimePoints(), supporter.getLastTimePoints());
        boolean right = !container.canAllBeBefore(consumer.getFirstTimePoints(), threat.getFirstTimePoints());
        return left && right;
    }

    private boolean mightIndirectlySupport(Timeline potentialSupporter, int changeNumber, Timeline consumer) {
        if(consumer == potentialSupporter)
            return false;

        if(!container.unifiable(potentialSupporter, consumer))
            return false;

        // if the consumer contains changes, the only possible support is the last change of the supporter
        if(!consumer.hasSinglePersistence() && changeNumber != potentialSupporter.numChanges()-1)
            return false;

        final ChainComponent supportingCC = potentialSupporter.getChangeNumber(changeNumber);
        if(!container.canAllBeBefore(supportingCC.getSupportTimePoint(), consumer.getFirstTimePoints()))
            return false;

        // if the supporter is not the last change, check that we can fit the consuming db before the next change
        // and that this change directly support the presistence (no statement can be added between the two)
        if(changeNumber < potentialSupporter.numChanges()-1) {
            final ChainComponent afterCC = potentialSupporter.getChangeNumber(changeNumber+1);
            if(!container.canAllBeBefore(consumer.getLastTimePoints(), afterCC.getConsumeTimePoint()))
                return false;

            if(!container.unifiable(supportingCC.getSupportValue(), consumer.getGlobalConsumeValue()))
                return false;
        }

        return true;
    }

    @Override
    public void timelineRemoved(Timeline tl) {
        if(potentialSupporters.containsKey(tl.mID)) {
            for (Event e : potentialSupporters.get(tl.mID)) {
                possiblyInterferingTimelines.remove(e);
            }
            potentialSupporters.remove(tl.mID);
        }
        removedTimelines.add(tl.mID);
    }

    @Override
    public void timelineAdded(Timeline a) {
        addedTimelines.add(a.mID);
    }

    @Override
    public void timelineExtended(Timeline tl) {
        extendedTimelines.add(tl.mID);
    }

    public void report() {
        TimelinesManager tdb = container.tdb;
        StringBuilder sb = new StringBuilder();
        for(int tlID : potentialSupporters.keySet()) {
            if(!tdb.containsTimelineWithID(tlID)) continue;
            sb.append(Printer.inlineTimeline(container, tlID));
            sb.append("\n");
            for(Event pis : potentialSupporters.get(tlID)) {
                if(!tdb.containsTimelineWithID(pis.supporterID)) continue;
                sb.append("  (");
                sb.append(pis.changeNumber);
                sb.append(")  ");
                sb.append(Printer.inlineTimeline(container, pis.supporterID));
                sb.append("\n");
            }
        }
        System.out.println(sb.toString());
    }
}
