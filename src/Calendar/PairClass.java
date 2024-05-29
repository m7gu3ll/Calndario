package Calendar;

public class PairClass<K extends Comparable<K>, V extends Comparable<V>> implements Pair<K, V> {

    private final K first;
    private final V second;

    public PairClass(K first, V second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public K first() {
        return first;
    }

    @Override
    public V second() {
        return second;
    }

    @Override
    public int compareTo(Pair<K, V> o) {
        int i = first().compareTo(o.first());
        if (i != 0)
            return i;
        return second().compareTo(o.second());
    }

    @Override
    public boolean equals(Object o) {
        return compareTo((Pair<K, V>) o) == 0;
    }
}
