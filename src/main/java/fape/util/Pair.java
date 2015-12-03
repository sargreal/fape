package fape.util;

public class Pair<T, V> {

    public T value1;
    public V value2;

    public Pair(T v1, V v2) {
        this.value1 = v1;
        this.value2 = v2;
    }

    @Override
    public String toString() {
        return "["+value1 + ","+value2+"]";
    }
    
    
}
