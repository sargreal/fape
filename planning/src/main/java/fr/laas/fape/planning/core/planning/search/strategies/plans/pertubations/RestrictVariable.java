package fr.laas.fape.planning.core.planning.search.strategies.plans.pertubations;

import fr.laas.fape.planning.core.planning.states.PartialPlan;
import fr.laas.fape.anml.model.concrete.VarRef;

import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;


public class RestrictVariable implements Pertubation {

  private final Random rnd;

  public RestrictVariable(Random rnd) {
    this.rnd = rnd;
  }
  
  public void apply(PartialPlan plan) { 

    // Get variables where the domain is greater than 1
    List<VarRef> variables = new ArrayList<>();
    for(VarRef var : plan.csp.bindings().variables()) {
      if(var != null && plan.domainSizeOf(var) > 1) {
        variables.add(var);
      }
    }

    if(variables.isEmpty()) {
      return;
    }

    // Select a random variable
    VarRef var = variables.get(rnd.nextInt(variables.size()));

    // Get the domain of the variable
    List<String> domain = new ArrayList<>();
    domain.addAll(plan.csp.bindings().domainOf(var));

    // Restrict the domain by removing one random possible value
    domain.remove(rnd.nextInt(domain.size()));
    plan.csp.bindings().restrictDomain(var, domain);
  }

}
