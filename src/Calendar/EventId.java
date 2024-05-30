package Calendar;

public class EventId implements Pair<String, String> {

    private final String first;
    private final String second;

    public EventId(String first, String second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public String first() {
        return first;
    }

    @Override
    public String second() {
        return second;
    }

    @Override
    public int compareTo(Pair<String, String> o) {
        int i = first().compareTo(o.first());
        if (i != 0)
            return i;
        return second().compareTo(o.second());
    }

    @Override
    public boolean equals(Object o) {
        return compareTo((EventId) o) == 0;
    }
}
