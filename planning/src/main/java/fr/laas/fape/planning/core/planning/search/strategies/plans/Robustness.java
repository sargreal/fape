package fr.laas.fape.planning.core.planning.search.strategies.plans;

import fr.laas.fape.planning.core.planning.states.PartialPlan;
import fr.laas.fape.planning.core.planning.search.strategies.plans.pertubations.DelayTimepoint;
import fr.laas.fape.planning.core.planning.search.strategies.plans.pertubations.Pertubation;
import fr.laas.fape.planning.core.planning.search.strategies.plans.pertubations.RestrictVariable;
import fr.laas.fape.anml.model.concrete.VarRef;
import fr.laas.fape.exceptions.InconsistencyException;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Robustness is a search strategy that aims to create the most robust plan possible. It is based on the idea that a plan
 * is more robust if it can withstand more pertubations. A more robust plan is more likely to be executed successfully.
 */
public class Robustness extends PartialPlanComparator {

    private final static int SEED = 0;
    private final static int MAX_PERTUBATION_ITERATIONS = 10; 
    private final static int MAX_PERTUBATIONS = 100;

    private final static Random rnd = new Random(SEED);

    @Override
    public String shortName() {
        return "robust";
    }

    @Override
    public String reportOnState(PartialPlan plan) {
        return String.format("Robustness:\t avg-pertubations-withstood: %s", eval(plan));
    }

    public double eval(PartialPlan plan) {
        // Generate several lists of pertubations on the plan
        
        List<Pertubation> pertubations = new ArrayList<>();
        pertubations.add(new DelayTimepoint(rnd));
        // pertubations.add(new RestrictVariable(rnd));
        List<Integer> robustnesses = new ArrayList<>();

        // Select a random variable
        for (int i = 0; i < MAX_PERTUBATION_ITERATIONS; i++) {
            int pertubationsWithstood = 0;
            PartialPlan planCopy = plan.cc(plan.mID+i);
            for (int j = 0; j < MAX_PERTUBATIONS; j++) {
                try {

                    // Choose random pertubation
                    Pertubation pertubation = pertubations.get(rnd.nextInt(pertubations.size()));
                    pertubation.apply(planCopy);
                    // Check if the plan is still valid
                    boolean consistent = planCopy.checkConsistency();
                    if (!consistent) {
                        throw new InconsistencyException();
                    }
                    // If it is, increment pertubationsWithstood
                    pertubationsWithstood++;
                } catch (InconsistencyException e) {
                    // If it is not, break
                    break;
                }
            }
            robustnesses.add(pertubationsWithstood);
        }

        // Return the average number of pertubations withstood
        double result = robustnesses.stream().mapToInt(Integer::intValue).average().orElse(0);
        // System.out.println("Robustness of plan "+plan.mID+": " + result);
        return result;
    }

    @Override
    public double g(PartialPlan plan) {
        return eval(plan);
    }

    @Override
    public double h(PartialPlan plan) {
        return 0f;
    }

    @Override
    public double hc(PartialPlan plan) {
        return 0f;
    }
}
