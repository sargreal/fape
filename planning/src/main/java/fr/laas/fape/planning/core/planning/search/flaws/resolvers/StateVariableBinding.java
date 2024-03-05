/*
 * Author:  Filip Dvorak <filip.dvorak@runbox.com>
 *
 * Copyright (c) 2013 Filip Dvorak <filip.dvorak@runbox.com>, all rights reserved
 *
 * Publishing, providing further or using this program is prohibited
 * without previous written permission of the author. Publishing or providing
 * further the contents of this file is prohibited without previous written
 * permission of the author.
 */

package fr.laas.fape.planning.core.planning.search.flaws.resolvers;

import fr.laas.fape.anml.model.ParameterizedStateVariable;
import fr.laas.fape.anml.model.concrete.Chronicle;
import fr.laas.fape.anml.model.concrete.VarEqualityConstraint;
import fr.laas.fape.planning.core.planning.states.PartialPlan;
import fr.laas.fape.planning.core.planning.states.modification.ChronicleInsertion;
import fr.laas.fape.planning.core.planning.states.modification.PartialPlanModification;

/**
 * Unifies both state variables. This is done in unifying all their parameters.
 */
public class StateVariableBinding implements Resolver {

    public final ParameterizedStateVariable a, b;

    public StateVariableBinding(ParameterizedStateVariable a, ParameterizedStateVariable b) {
        assert a.args().length == b.args().length;
        assert a.func() == b.func() : "Trying two unify two state variables with different functions.";
        this.a = a;
        this.b = b;
    }

    @Override
    public PartialPlanModification asStateModification(PartialPlan partialPlan) {
        Chronicle chronicle = new Chronicle();
		chronicle.setOrigin("StateVariableBinding");
        for(int i=0 ; i<a.args().length ; i++)
            chronicle.addConstraint(new VarEqualityConstraint(a.arg(i), b.arg(i)));
        return new ChronicleInsertion(chronicle);
    }

    @Override
    public int compareWithSameClass(Resolver e) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
