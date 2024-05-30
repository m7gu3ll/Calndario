package Calendar;

import Exceptions.*;

import java.time.LocalDateTime;
import java.util.Iterator;

public interface CalendarSystem {
    /**
     * checks if an Account with a certain name exists
     * @param name name of the Account
     * @return true if the Account exists and false otherwise
     */
    boolean hasAccount(String name);

    /**
     * checks if an Event with a certain name exists
     * @param name name of the Event
     * @param promoter promoter of the Event
     * @return true if the Event exists and false otherwise
     */
    boolean hasEvent(String name, String promoter);

    /**
     * checks if a type of Account is valid
     * @param type of the Account
     * @return true if it's invalid and false otherwise
     */
    boolean typeIsInvalid(String type);

    /**
     * registers a new Account
     * @param name name of the Account
     * @param type type of the Account
     * @throws AccountAlreadyExists
     * @throws InvalidType
     */
    void register(String name, String type) throws AccountAlreadyExists, InvalidType;

    /**
     * returns an iterator with every Account registered in the system
     * @return an iterator with every Account registered
     */
    Iterator<Account> accounts();

    Iterator<Event> events();

    /**
     * returns an iterator with all the events with at least one of the topics in "topics"
     * @param topics topics of the events
     * @return an iterator with all the events with at least one of the topics in "topics"
     */
    Iterator<Event> topics(String[] topics);

    /**
     * creates a new Event
     * @param accountName name of the promoter of the Event
     * @param eventName name of the Event
     * @param priority priority of the Event
     * @param date date of the Event
     * @param topics topics of the Event
     * @throws AccountDoesntExist
     * @throws UnknownPriority
     * @throws GuestsCantCreateEvents
     * @throws StaffCantCreateHighPriorityEvents
     * @throws EventAlreadyExists
     * @throws HasAnotherEventAtThatTime
     */
    void create(String accountName, String eventName, String priority, LocalDateTime date, String[] topics) throws AccountDoesntExist, UnknownPriority, GuestsCantCreateEvents, StaffCantCreateHighPriorityEvents, EventAlreadyExists, HasAnotherEventAtThatTime;

    /**
     * invites an Account to an Event
     * @param invited invited Account
     * @param promoter promoter of the Event
     * @param eventName name of the Event
     * @pre invited exists, promoter exists and event exists
     * @return an iterator whose first element is null followed all the Events rejected if the
     * Event is of High priority and an empty iterator otherwise;
     * @throws AccountDoesntExist
     * @throws EventDoesntExist
     * @throws AlreadyInvitedToThatEvent
     * @throws HasAnotherEventAtThatTime
     */
    Iterator<Event> invite(String invited, String promoter, String eventName) throws AccountDoesntExist, EventDoesntExist, AlreadyInvitedToThatEvent, HasAnotherEventAtThatTime;

    /**
     * returns an iterator with the ids (name and promoter) of all Events to which the Account was invited to
     * @param account name of the said Account
     * @return an iterator with the ids (name and promoter) of all Events to which the Account was invited to
     * @throws AccountDoesntExist
     */
    Iterator<Pair<String,String>> getAccountEvents(String account) throws AccountDoesntExist;

    /**
     * checks if the Account is occupied at a certain date
     * @param accountName name of the account
     * @param date certain date
     * @return true if the Account exists
     */
    boolean accountIsOccupied(String accountName, LocalDateTime date);

    /**
     * responds to an Event on behalf of a certain Account
     * @param invited certain Account
     * @param promoter promoter of the Event
     * @param eventName name of the Event
     * @param responseName response
     * @pre account exists
     * @return an iterator with all the Events rejected
     * @throws AccountDoesntExist
     * @throws InvalidResponse
     * @throws NoInvitation
     * @throws EventDoesntExist
     * @throws AlreadyRespondedToThatEvent
     * @throws HasAnotherEventAtThatTime
     */
    Iterator<Event> response(String invited, String promoter, String eventName, String responseName) throws AccountDoesntExist, InvalidResponse, NoInvitation, EventDoesntExist, AlreadyRespondedToThatEvent, HasAnotherEventAtThatTime;

    /**
     * returns the number of invites of an Event
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return the number of invites of an Event
     */
    int getNOInvites(String event, String promoter);

    /**
     * returns the number of rejected invites of an Event
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return the number of rejected invites of an Event
     */
    int getNORejectedInvites(String event, String promoter);

    /**
     * returns the number of accepted invites of an Event
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return the number of accepted invites of an Event
     */
    int getNOAcceptedInvites(String event, String promoter);

    /**
     * returns the number of unanswered invites of an Event
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return the number of unanswered invites of an Event
     */
    int getNOUnansweredInvites(String event, String promoter);

    /**
     * returns the date of an Event
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return the date of an Event
     * @throws AccountDoesntExist
     * @throws EventDoesntExist
     */
    LocalDateTime getEventDate(String event, String promoter) throws AccountDoesntExist, EventDoesntExist;

    /**
     * returns an iterator with the names of the Accounts invited
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return an iterator with the names of the Accounts invited
     */
    Iterator<String> getEventInvited(String event, String promoter);

    /**
     * returns an iterator with the responses of the Accounts invited
     * @param event name of the Event
     * @param promoter promoter of the Event
     * @pre event exists
     * @return an iterator with the responses of the Accounts invited
     */
    Iterator<Boolean> getEventResponses(String promoter, String event);
}
