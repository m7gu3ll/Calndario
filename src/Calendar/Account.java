package Calendar;

import java.util.Iterator;

public interface Account {
    void getInviteTo(Pair<String, String> event);

    void accept(Pair<String, String> event);

    void reject(Pair<String, String> event);

    boolean wasInvitedTo(Pair<String, String> event);

    Iterator<Pair<String, String>> getEvents();

    String getName();

    String getType();

    void remove(Pair<String, String> event);

    Boolean getResponseTo(Pair<String, String> eventName);

    void promote(Pair<String, String> id);
}