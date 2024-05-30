package Calendar;

import java.util.*;

public abstract class AccountClass implements Account {
    private final List<Pair<String, String>> events;
    private final List<Boolean> responses;
    private final String name;
    private final String type;

    public AccountClass(String name, String type) {
        events = new ArrayList<>();
        responses = new ArrayList<>();
        this.name = name;
        this.type = type;
    }

    @Override
    public void getInviteTo(Pair<String, String> event) {
        events.add(event);
        responses.add(null);
    }

    @Override
    public void accept(Pair<String, String> event) {
        responses.set(events.indexOf(event), true);
    }

    @Override
    public void reject(Pair<String, String> event) {
        responses.set(events.indexOf(event), false);
    }

    @Override
    public void remove(Pair<String, String> event) {
        int i = events.indexOf(event);
        events.remove(i);
        responses.remove(i);
    }

    @Override
    public void promote(Pair<String, String> id) {
        getInviteTo(id);
        accept(id);
    }

    @Override
    public boolean wasInvitedTo(Pair<String, String> event) {
        return events.contains(event);
    }

    @Override
    public Boolean getResponseTo(Pair<String, String> eventName) {
        int i = events.indexOf(eventName);
        return responses.get(i);
    }

    @Override
    public Iterator<Pair<String, String>> getEvents() {
        return events.iterator();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getType() {
        return type;
    }
}
