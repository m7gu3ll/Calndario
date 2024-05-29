package Calendar;

import java.time.LocalDateTime;
import java.util.Iterator;

public interface Event extends Comparable<Event> {
    String getName();

    String getPromoter();

    int getPriority();

    LocalDateTime getDate();

    Iterator<String> getTopics();

    int getNOInvites();

    int getNOAcceptedInvites();

    int getNORejectedInvites();

    int getNOUnansweredInvites();

    void invite(String name);

    void getAccepted(String name);

    void getRejected(String name);

    Iterator<String> getInvited();

    Iterator<Boolean> getResponses();

    int numberOfMatchingTopics(String[] topics);

}
