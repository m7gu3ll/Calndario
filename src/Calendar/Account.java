package Calendar;

import java.util.Iterator;

public interface Account {
    /**
     * receives an invitation to an Event
     * @param event id of said Event
     * @pre there's an Event with id
     */
    void getInviteTo(Pair<String, String> event);

    /**
     * accepts an invitation to an Event
     * @param event id of said Event
     * @pre there's an Event with id
     */
    void accept(Pair<String, String> event);

    /**
     * rejects an invitation to an Event
     * @param event id of said Event
     * @pre there's an Event with id
     */
    void reject(Pair<String, String> event);

    /**
     * checks if the Account was invited to an Event
     * @param event id of said Event
     * @pre there's an Event with id
     * @return true if the Account was invited to said Event and false otherwise
     */
    boolean wasInvitedTo(Pair<String, String> event);

    /**
     * returns an iterator with the ids of the Events the Account was invited to
     * @return
     */
    Iterator<Pair<String, String>> getEvents();

    /**
     * returns the name of the Account
     * @return the name of the Account
     */
    String getName();
    /**
     * returns the type of the Account
     * @return the type of the Account
     */
    String getType();

    /**
     * removes the invitation of an Event
     * @param id id of the Event
     * @pre there's an Event with id
     */
    void remove(Pair<String, String> id);

    /**
     * returns the response to a certain Event
     * @param id name of the Event
     * @pre there's an Event with id
     * @return the response to a certain Event
     */
    Boolean getResponseTo(Pair<String, String> id);

    /**
     * sets up the Account as the promoter of an Event
     * @param id of the Event
     * @pre there's an Event with id
     */
    void promote(Pair<String, String> id);
}