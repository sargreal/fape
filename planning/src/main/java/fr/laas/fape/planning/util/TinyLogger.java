package fr.laas.fape.planning.util;

import fr.laas.fape.planning.core.planning.states.Printer;
import fr.laas.fape.planning.core.planning.states.PartialPlan;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TinyLogger {
    public static boolean logging = false;

    public static final Logger logger = LoggerFactory.getLogger(TinyLogger.class);

    public static void LogInfo(PartialPlan st, String toFormat, Object... objects) {
        if(!logging)
            return;

        Object[] printables = new Object[objects.length];
        for(int i=0 ; i<objects.length ; i++) {
            printables[i] = Printer.stateDependentPrint(st, objects[i]);
        }

        logger.debug(String.format(toFormat, printables));
    }

    //public static boolean logging = false;
    public static void LogInfo(String st) {
        if (logging) {
            logger.debug(st);
        }
    }

    public static void LogInfo(Reporter o) {
        if(logging) {
            logger.debug(o.report());
        }
    }
}
