package fr.laas.fape.planning.exceptions;

public class FAPEException extends RuntimeException {
    private static final long serialVersionUID = 142546216423654L;
    public FAPEException(String st) {
        super(st);
    }
    public FAPEException(String st, Throwable t) {
        super(st, t);
    }
}
