package fr.laas.fape.planning.core.planning.search.strategies.plans.pertubations;

import fr.laas.fape.planning.core.planning.states.PartialPlan;
import fr.laas.fape.anml.model.concrete.TPRef;
import fr.laas.fape.structures.IList;
import fr.laas.fape.anml.pending.IntExpression;
import fr.laas.fape.anml.model.concrete.MinDelayConstraint;


import java.util.Random;
import java.util.List;

public class DelayTimepoint implements Pertubation {

  private final Random rnd;

  public DelayTimepoint(Random rnd) {
    this.rnd = rnd;
  }
  
  public void apply(PartialPlan plan) {
    IList<TPRef> adjustableTimepoints = plan.csp.stn().timepoints()
      .filter(tp -> ! (tp == plan.pb.start() || tp == plan.pb.end()))
      .filter(tp -> tp.genre().isDispatchable());

    if(adjustableTimepoints.size() <= 0) {
      return;
    }

    TPRef tp = adjustableTimepoints.get(rnd.nextInt(adjustableTimepoints.size()));
    TPRef start = plan.pb.start();

    int tpStart = plan.getEarliestStartTime(tp);
    int end = plan.getEarliestStartTime(plan.pb.end());

    int maxDelay = end - tpStart;

    // delay the timepoint by a random amount, between 1 and the current end time. Prefer smaller delays using a gaussian distribution
    int delay = (int) Math.abs(Math.round(rnd.nextGaussian() * (end - tpStart))) + 1;
    // int delay = (maxDelay > 0 ? rnd.nextInt(maxDelay) : 0) + 1;
    // int delay = 1;
    plan.csp.stn().enforceMinDelay(start, tp, delay);
  }

}
