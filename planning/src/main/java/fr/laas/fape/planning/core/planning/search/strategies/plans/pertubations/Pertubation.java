package fr.laas.fape.planning.core.planning.search.strategies.plans.pertubations;

import fr.laas.fape.planning.core.planning.states.PartialPlan;

public interface Pertubation {
  
  public void apply(PartialPlan plan);

}
