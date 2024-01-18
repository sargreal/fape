package fr.laas.fape;

public interface UniquelyIdentified {

    /**
     * An integer that gives a unique identifier to this object.
     * This id will then be used for primitive int to X hash maps
     */
    int id();
}
