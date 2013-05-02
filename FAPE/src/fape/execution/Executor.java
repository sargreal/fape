/*
 * Author:  Filip Dvořák <filip.dvorak@runbox.com>
 *
 * Copyright (c) 2013 Filip Dvořák <filip.dvorak@runbox.com>, all rights reserved
 *
 * Publishing, providing further or using this program is prohibited
 * without previous written permission of the author. Publishing or providing
 * further the contents of this file is prohibited without previous written
 * permission of the author.
 */
package fape.execution;

import fape.acting.Actor;
import fape.exceptions.FAPEException;
import fape.model.AtomicAction;
import fape.model.compact.ANMLBlock;
import fape.model.compact.ANMLFactory;
import fape.planning.Planner;
import fape.util.Pair;
import fape.util.TimePoint;
import gov.nasa.anml.Main;
import java.io.IOException;
import java.util.List;
import java.util.logging.Logger;
import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.tree.Tree;

/**
 *
 * @author FD
 */
public class Executor {

    Actor mActor;
    Listener mListener;

    public void bind(Actor a, Listener l) {
        mActor = a;
        mListener = l;
    }

    public ANMLBlock ProcessANMLfromFile(String path) {
        ANMLBlock b;
        try {
            Tree t = Main.getTree(path);
            b = ANMLFactory.Parse(t);
        } catch (RecognitionException | IOException e) {
            throw new FAPEException("System failed to read path: " + path);
        }
        return b;
    }

    /**
     * performs the translation between openPRS and ANML model
     *
     * @param message
     */
    public void eventReceived(String message) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /**
     * performs the translation between openPRS and ANML model
     *
     * @param acts
     */
    public void executeAtomicActions(List<Pair<AtomicAction, TimePoint>> acts) {
        throw new UnsupportedOperationException("Not yet implemented");
    }
}
