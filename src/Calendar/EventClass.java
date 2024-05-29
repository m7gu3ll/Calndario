package Calendar;

import java.time.LocalDateTime;
import java.util.*;

public class EventClass implements Event {
    private final String name;
    private final String promoter;
    private final int priority;
    private final LocalDateTime date;
    private final String[] topics;
    private final List<String> invites;
    private final List<Boolean> responses;

    public EventClass(String name, String promoter, int priority, LocalDateTime date, String[] topics) {
        this.name = name;
        this.promoter = promoter;
        this.priority = priority;
        this.date = date;
        this.topics = topics;
        invites = new ArrayList<>();
        responses = new ArrayList<>();
        invites.add(promoter);
        responses.add(true);
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getPromoter() {
        return promoter;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public LocalDateTime getDate() {
        return date;
    }

    @Override
    public Iterator<String> getTopics() {
        return Arrays.stream(topics).iterator();
    }

    @Override
    public int getNOInvites() {
        return invites.size();
    }

    @Override
    public int getNOAcceptedInvites() {
        Iterator<Boolean> it = responses.iterator();
        int i = 0;
        while (it.hasNext()) {
            Boolean wasInvited = it.next();
            if (wasInvited != null)
                if (wasInvited)
                    i++;
        }
        return i;
    }

    @Override
    public int getNORejectedInvites() {
        Iterator<Boolean> it = responses.iterator();
        int i = 0;
        while (it.hasNext()) {
            Boolean wasInvited = it.next();
            if (wasInvited != null)
                if (!wasInvited)
                    i++;
        }
        return i;
    }

    @Override
    public int getNOUnansweredInvites() {
        Iterator<Boolean> it = responses.iterator();
        int i = 0;
        while (it.hasNext())
            if (it.next() == null)
                i++;
        return i;
    }

    @Override
    public int compareTo(Event o) {
        return new PairClass<>(name, promoter).compareTo(new PairClass<>(o.getName(), o.getPromoter()));
    }

    @Override
    public void invite(String name) {
        invites.add(name);
        responses.add(null);
    }

    @Override
    public void getAccepted(String name) {
        responses.set(invites.indexOf(name), true);
    }

    @Override
    public void getRejected(String name) {
        responses.set(invites.indexOf(name), false);
    }

    @Override
    public Iterator<String> getInvited() {
        return invites.iterator();
    }

    @Override
    public Iterator<Boolean> getResponses() {
        return responses.iterator();
    }

    @Override
    public int numberOfMatchingTopics(String[] topics) {
        int n = 0;
        for (int i = 0; i < topics.length; i++) {
            if (topicsContains(topics[i]))
                n++;
        }
        return n;
    }

    private boolean topicsContains(String topic) {
        boolean contains = false;
        int i = 0;
        while (i < topics.length && !contains) {
            if (topics[i].equals(topic))
                contains = true;
            i++;
        }
        return contains;
    }
}
