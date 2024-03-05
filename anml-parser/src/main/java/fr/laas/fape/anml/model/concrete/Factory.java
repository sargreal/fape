package fr.laas.fape.anml.model.concrete;

import fr.laas.fape.anml.model.AnmlProblem;
import fr.laas.fape.anml.model.abs.AbstractAction;

/**
 * This class provides static factory method for creating concrete objects from abstract ones.
 *
 * It is mainly a Java-friendly wrapper around the factories defined as Scala companion objects.
 */
public class Factory {

    /**
     * Returns a new action as part of a problem.
     * @param pb Problem in which the action appears
     * @param abs Abstract version of the action.
     * @return A fully instantiated Action
     */
    public static Action getStandaloneAction(AnmlProblem pb, AbstractAction abs, RefCounter refCounter) {
        return Action$.MODULE$.getNewStandaloneAction(pb, abs, refCounter);
    }

    
    /**
     * Returns a new action as part of a problem.
     * @param pb Problem in which the action appears
     * @param abs Abstract version of the action.
     * @return A fully instantiated Action
     */
    public static Action getSupportingAction(AnmlProblem pb, AbstractAction abs, Task supportedAction, RefCounter refCounter) {
        return Action$.MODULE$.getNewSupportingAction(pb, abs, supportedAction, refCounter);
    }

    /**
     * Creates a new action with some predefined arguments and an identifier.
     * Do not define an identifier yourself unless you know what you're doing.
     * @param pb Problem in which the action appears.
     * @param abs AbstractAction to make concrete.
     * @param parameters List of parameters.
     * @param id ID of the action
     * @return A concrete action with the given parameters.
     *
    public static Action getInstantiatedAction(AnmlProblem pb, AbstractAction abs, List<VarRef> parameters, ActRef id) {
        return Action$.MODULE$.jNewAction(pb, abs, parameters, new LActRef(), id, null, null);
    }*/
}
