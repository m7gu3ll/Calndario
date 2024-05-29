package Calendar;

public interface Pair<K,V> extends Comparable<Pair<K,V>>{
   K first();
   V second();
}
