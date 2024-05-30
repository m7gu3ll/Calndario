package Calendar;

import java.time.LocalDateTime;
import java.util.Iterator;

public interface Event extends Comparable<Event> {
    /**
     * returns the name of the Event
     * @return the name of the Event
     */
    String getName();

    /**
     * returns the promoter of the Event
     * @return the promoter of the Event
     */
    String getPromoter();

    /**
     * returns the priority of the Event
     * @return the priority of the Event
     */
    int getPriority();

    /**
     * returns the date of the Event
     * @return the date of the Event
     */
    LocalDateTime getDate();

    /**
     * returns an iterator with the topics of the Event
     * @return an iterator with the topics of the Event
     */
    Iterator<String> getTopics();

    /**
     * returns the number of invites of the Event
     * @return the number of invites of the Event
     */
    int getNOInvites();

    /**
     * returns the number of accepted invites of the Event
     * @return the number of accepted invites of the Event
     */
    int getNOAcceptedInvites();

    /**
     * returns the number of rejected invites of the Event
     * @return the number of rejected invites of the Event
     */
    int getNORejectedInvites();

    /**
     * returns the number of unanswered invites of the Event
     * @return the number of unanswered invites of the Event
     */
    int getNOUnansweredInvites();

    /**
     * invites an Account
     * @param name name of the Account
     * @pre there's an Account named name
     */
    void invite(String name);

    /**
     * gets accepted by an Account
     * @param name name of the Account
     * @pre there's an Account named name
     */
    void getAccepted(String name);

    /**
     * gets rejected by an Account
     * @param name name of the Account
     * @pre there's an Account named name
     */
    void getRejected(String name);

    /**
     * returns all the invites of the Event
     * @return all the invites of the Event
     */
    Iterator<String> getInvited();

    /**
     * returns all the responses of the Event
     * @return all the responses of the Event
     */
    Iterator<Boolean> getResponses();

    /**
     * returns the number of topics matching the Events topics
     * @param topics topics to verify
     * @return returns the number of topics matching the Events topics
     */
    int numberOfMatchingTopics(String[] topics);

}
