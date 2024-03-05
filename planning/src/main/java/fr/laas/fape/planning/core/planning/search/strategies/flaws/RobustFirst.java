package fr.laas.fape.planning.core.planning.search.strategies.flaws;

import fr.laas.fape.planning.core.planning.planner.Planner;
import fr.laas.fape.planning.core.planning.search.flaws.flaws.Flaw;
import fr.laas.fape.planning.core.planning.search.flaws.flaws.Threat;
import fr.laas.fape.planning.core.planning.search.flaws.resolvers.*;
import fr.laas.fape.planning.core.planning.states.PartialPlan;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import lombok.Value;

/**
 * This strategies orders flaws by the expected robustness of the resolvers.
 * The robustness of a resolver is only defined for 
 */
@Value
class RobustFirst implements FlawComparator, Comparator<Flaw> {

  private final PartialPlan st;
  private final Planner planner;

  private static final Map<Flaw, Integer> robustnessCache = new HashMap<>();

  @Override
  public String shortName() {
    return "robust";
  }

  private int calculateRobustness(Resolver res) {
    int robustness = 0;
    if (res instanceof TemporalSeparation) {
      robustness += 1;
    }
    return robustness;
  }

  private int getRobustness(Flaw f) {
    if (robustnessCache.containsKey(f)) {
      return robustnessCache.get(f);
    }
    if (!(f instanceof Threat)) {
      return 0;
    }
    int robustness = 0;
    for (Resolver res : f.getResolvers(st, planner)) {
      robustness = Math.max(robustness, calculateRobustness(res));
    }
    robustnessCache.put(f, robustness);
    return robustness;
  }

  @Override
  public int compare(Flaw f1, Flaw f2) {
    return getRobustness(f1) - getRobustness(f2);
  }
}
