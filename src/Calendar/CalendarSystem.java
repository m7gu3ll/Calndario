package Calendar;

import Exceptions.*;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;

public interface CalendarSystem {
    boolean hasUser(String name);
    boolean hasEvent(String name, String promoter);

    boolean typeIsInvalid(String type);

    void register(String name, String type) throws UserAlreadyExists, InvalidType;

    Iterator<Account> accounts();

    Iterator<Event> OrderedEvents(ArrayList<String> topics);

    void create(String accountName, String eventName, String priority, LocalDateTime date, String[] topics) throws UserDoesntExist, UnknownPriority, GuestsCantCreateEvents, StaffCantCreateHighPriorityEvents, EventAlreadyExists, HasAnotherEventAtThatTime;

    Iterator<Event> invite(String invited, String promoter, String eventName) throws UserDoesntExist, EventDoesntExist, AlreadyInvitedToThatEvent, HasAnotherEventAtThatTime;

    Iterator<Pair<String,String>> getAccountEvents(String user) throws UserDoesntExist;

    boolean userIsOccupied(String user, LocalDateTime date);

    Iterator<Event> response(String invited, String promoter, String eventName, String responseName) throws UserDoesntExist, InvalidResponce, NoInvitation, EventDoesntExist, AlreadyRespondedToThatEvent, UserIsOcuppied;

    int getNOInvites(String event, String promoter);

    int getNORejectedInvites(String event, String promoter);

    int getNOAcceptedInvites(String event, String promoter);

    int getNOUnansweredInvites(String event, String promoter);

    LocalDateTime getEventDate(String event, String s) throws UserDoesntExist, EventDoesntExist;

    Iterator<String> getEventInvited(String event, String s);

    Iterator<Boolean> getEventResponses(String promoter, String event);
}
