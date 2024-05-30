package Calendar;

import Exceptions.*;
import Utilities.Sort;

import java.time.LocalDateTime;
import java.util.*;

public class CalendarSystemClass implements CalendarSystem {
    public static final String STAFF = "STAFF";
    public static final String MANAGER = "MANAGER";
    public static final String GUEST = "GUEST";
    public static final String HIGH = "high";
    public static final String MID = "mid";
    public static final int PROMOTER_MISSING = 1;
    public static final int INVITEE_MISSING = 2;
    public static final int PROMOTER_AND_INVITEE_MISSING = 3;
    public static final String ACCEPT = "accept";
    public static final String REJECT = "reject";
    Map<String, Account> users;
    Map<Pair<String, String>, Event> events;

    public CalendarSystemClass() {
        users = new TreeMap<>();
        events = new TreeMap<>();
    }

    @Override
    public boolean hasUser(String name) {
        return users.containsKey(name);
    }

    @Override
    public boolean hasEvent(String name, String promoter) {
        return events.containsKey(id(name, promoter));
    }

    @Override
    public boolean typeIsInvalid(String type) {
        switch (type) {
            case STAFF, MANAGER, GUEST -> {
                return false;
            }
            default -> {
                return true;
            }
        }
    }

    @Override
    public void register(String name, String type) throws UserAlreadyExists, InvalidType {
        if (hasUser(name))
            throw new UserAlreadyExists();
        if (typeIsInvalid(type))
            throw new InvalidType();

        Account newAccount = null;
        switch (type) {
            case GUEST -> newAccount = new Guest(name);
            case STAFF -> newAccount = new Staff(name);
            case MANAGER -> newAccount = new Manager(name);
        }
        users.put(name, newAccount);
    }

    @Override
    public Iterator<Account> accounts() {
        return users.values().iterator();
    }

    public Iterator<Event> events() {
        return events.values().iterator();
    }

    @Override
    public Iterator<Event> topics(String[] topics) {
        ArrayList<Event> events = new ArrayList<>();
        int totalSize = topics.length;
        while (totalSize > 0) {
            List<Event> temp = eventsWithTopicsInCommon(topics, totalSize);
            for (int i = 0; i < temp.size(); i++) {
                events.add(temp.get(i));
            }
            totalSize--;
        }
        return events.iterator();
    }


    private List<Event> eventsWithTopicsInCommon(String[] topics, int numberOfTopics) {
        List<Event> events = new ArrayList<>();

        Iterator<Event> it = events();
        while (it.hasNext()) {
            Event event = it.next();
            if (event.numberOfMatchingTopics(topics) == numberOfTopics) {
                events.add(event);
            }
        }
        Sort.sort(events);
        return events;
    }

    private void getInvited(Event event, Account user) {
        user.getInviteTo(id(event.getName(), event.getPromoter()));
        event.invite(user.getName());
    }

    private Iterator<Event> acceptInvite(Event event, Account user) {
        List<Event> rejectedEvents = rejectAllThatConflictWith(event, user);
        user.accept(id(event.getName(), event.getPromoter()));
        event.getAccepted(user.getName());
        return rejectedEvents.iterator();
    }


    private void rejectInvite(Event event, Account user) {
        user.reject(id(event.getName(), event.getPromoter()));
        event.getRejected(user.getName());
    }

    private List<Event> rejectAllThatConflictWith(Event event, Account user) {
        Iterator<Pair<String, String>> it = user.getEvents();
        List<Event> rejectedEvents = new ArrayList<>();
        while (it.hasNext()) {
            Event otherEvent = events.get(it.next());
            if (conflicts(otherEvent, event) &&
                    !Boolean.FALSE.equals(user.getResponseTo(id(otherEvent.getName(), otherEvent.getPromoter())))) {
                rejectInvite(otherEvent, user);
                rejectedEvents.add(otherEvent);
            }
        }
        return rejectedEvents;
    }

    private void cancel(Event event) {
        Iterator<String> it = event.getInvited();
        while (it.hasNext()) {
            users.get(it.next()).remove(id(event.getName(), event.getPromoter()));
        }
        events.remove(id(event.getName(), event.getPromoter()));
    }

    private void promote(Pair<String, String> id) {
        Event event = events.get(id);
        Account user = users.get(event.getPromoter());
        rejectAllThatConflictWith(event, user);
        user.promote(id);
    }

    @Override
    public void create(String accountName, String eventName, String priorityName, LocalDateTime date,
                       String[] topics) throws UserDoesntExist, UnknownPriority,
            GuestsCantCreateEvents, StaffCantCreateHighPriorityEvents, EventAlreadyExists,
            HasAnotherEventAtThatTime {
        Pair<String, String> id = id(eventName, accountName);
        if (!hasUser(accountName))
            throw new UserDoesntExist(null);
        Account promoter = users.get(accountName);
        int priority = 0;
        switch (priorityName) {
            case HIGH:
                if (promoter instanceof Staff)
                    throw new StaffCantCreateHighPriorityEvents();
                priority++;
            case MID:
                priority++;
                break;
            default:
                throw new UnknownPriority();
        }
        if (promoter instanceof Guest)
            throw new GuestsCantCreateEvents();
        if (events.get(id) != null)
            throw new EventAlreadyExists();
        if (userIsOccupied(accountName, date))
            throw new HasAnotherEventAtThatTime();

        events.put(id, new EventClass(eventName, accountName, priority, date, topics));
        promote(id);
    }

    @Override
    public Iterator<Event> invite(String invited, String promoter, String eventName) throws UserDoesntExist, EventDoesntExist, AlreadyInvitedToThatEvent, HasAnotherEventAtThatTime {
        checkIfAccountsExist(invited, promoter);

        Event event = events.get(id(eventName, promoter));
        Account user = users.get(invited);
        if (event == null)
            throw new EventDoesntExist();
        if (!event.getPromoter().equals(promoter))
            throw new EventDoesntExist();
        if (user == null)
            throw new AlreadyInvitedToThatEvent();
        if (user.wasInvitedTo(id(eventName, promoter)))
            throw new AlreadyInvitedToThatEvent();

        return invite(user, event);
    }

    private Iterator<Event> invite(Account user, Event event) throws HasAnotherEventAtThatTime {
        if (event.getPriority() == 2 && user instanceof Staff) {
            return getInvitedToHighPriorityEvent(user, event);
        } else {
            checkForEventsAtTheSameTime(event, user);
            getInvited(event, user);
        }
        return noEvents();
    }

    private Iterator<Event> getInvitedToHighPriorityEvent(Account user, Event event) throws HasAnotherEventAtThatTime {
        List<Event> rejectedEvents = new ArrayList<>();
        rejectedEvents.add(null);
        Iterator<Pair<String, String>> it = user.getEvents();
        boolean foundHighPriorityEvent = false;
        Event canceledEvent = null;
        while (it.hasNext() && !foundHighPriorityEvent) {
            Pair<String, String> id = it.next();
            Event otherEvent = events.get(id(id.first(), id.second()));
            if (conflicts(otherEvent, event) &&
                    !Boolean.FALSE.equals(user.getResponseTo(id(otherEvent.getName(), otherEvent.getPromoter())))) {
                if (otherEvent.getPriority() == 2) {
                    rejectedEvents.set(0, event);
                    getInvited(event, user);
                    rejectInvite(event, user);
                    foundHighPriorityEvent = true;
                } else if (otherEvent.getPromoter().equals(user.getName())) {
                    rejectedEvents.add(otherEvent);
                    canceledEvent = otherEvent;
                } else {
                    rejectedEvents.add(otherEvent);
                    rejectInvite(otherEvent, user);
                }
            }
        }
        if (!foundHighPriorityEvent) {
            getInvited(event, user);
            acceptInvite(event, user);
        } else {
            throw new HasAnotherEventAtThatTime();
        }
        if (canceledEvent != null)
            cancel(canceledEvent);
        return rejectedEvents.iterator();
    }

    private void checkForEventsAtTheSameTime(Event event, Account user) throws HasAnotherEventAtThatTime {
        Iterator<Pair<String, String>> it = user.getEvents();
        boolean foundEvent = false;
        while (it.hasNext() && !foundEvent) {
            Pair<String, String> id = it.next();
            Event otherEvent = events.get(id(id.first(), id.second()));
            if (conflicts(otherEvent, event))
                if (Boolean.TRUE.equals(user.getResponseTo(id)))
                    foundEvent = true;
        }
        if (foundEvent)
            throw new HasAnotherEventAtThatTime();
    }

    @Override
    public Iterator<Event> response(String invitedName, String promoterName, String eventName, String responseName) throws UserDoesntExist, InvalidResponce, NoInvitation, EventDoesntExist, AlreadyRespondedToThatEvent, HasAnotherEventAtThatTime {
        boolean accepted;
        checkIfAccountsExist(invitedName, promoterName);
        switch (responseName) {
            case ACCEPT -> accepted = true;
            case REJECT -> accepted = false;
            default -> throw new InvalidResponce();
        }
        Account invited = users.get(invitedName);
        Event event = events.get(id(eventName, promoterName));
        if (event == null)
            throw new EventDoesntExist();
        if (!invited.wasInvitedTo(id(eventName, promoterName)))
            throw new NoInvitation();
        if (invited.getResponseTo(id(eventName, promoterName)) != null)
            throw new AlreadyRespondedToThatEvent();

        if (!accepted) {
            rejectInvite(event, invited);
            return noEvents();
        }
        checkForEventsAtTheSameTime(event, invited);
        return acceptInvite(event, invited);
    }

    private void checkIfAccountsExist(String invitedName, String promoterName) throws UserDoesntExist {
        int usersMissing = 0;
        if (!hasUser(promoterName))
            usersMissing++;
        if (!hasUser(invitedName))
            usersMissing += 2;
        switch (usersMissing) {
            case PROMOTER_MISSING -> throw new UserDoesntExist("p");
            case INVITEE_MISSING -> throw new UserDoesntExist("i");
            case PROMOTER_AND_INVITEE_MISSING -> throw new UserDoesntExist("pi");
        }
    }

    private Iterator<Event> noEvents() {
        return new ArrayList<Event>().iterator();
    }

    private boolean conflicts(Event a, Event b) {
        return a.getDate().equals(b.getDate()) && a != b;
    }

    @Override
    public int getNOInvites(String event, String promoter) {
        return events.get(id(event, promoter)).getNOInvites();
    }

    @Override
    public int getNORejectedInvites(String event, String promoter) {
        return events.get(id(event, promoter)).getNORejectedInvites();
    }

    @Override
    public int getNOAcceptedInvites(String event, String promoter) {
        return events.get(id(event, promoter)).getNOAcceptedInvites();
    }

    @Override
    public int getNOUnansweredInvites(String event, String promoter) {
        return events.get(id(event, promoter)).getNOUnansweredInvites();
    }

    @Override
    public LocalDateTime getEventDate(String event, String promoter) throws UserDoesntExist, EventDoesntExist {
        if (users.get(promoter) == null)
            throw new UserDoesntExist(null);
        if (events.get(id(event, promoter)) == null)
            throw new EventDoesntExist();
        return events.get(id(event, promoter)).getDate();
    }

    @Override
    public Iterator<String> getEventInvited(String event, String promoter) {
        return events.get(id(event, promoter)).getInvited();
    }

    @Override
    public Iterator<Boolean> getEventResponses(String event, String promoter) {
        return events.get(id(event, promoter)).getResponses();
    }

    @Override
    public Iterator<Pair<String, String>> getAccountEvents(String accountName) throws UserDoesntExist {
        if (!hasUser(accountName))
            throw new UserDoesntExist(null);

        return users.get(accountName).getEvents();
    }

    @Override
    public boolean userIsOccupied(String userName, LocalDateTime date) {
        Account user = users.get(userName);
        Iterator<Pair<String, String>> it = user.getEvents();
        boolean isOccupied = false;
        while (it.hasNext() && !isOccupied) {
            Pair<String, String> event = it.next();
            if (events.get(event).getDate().equals(date)) {
                Boolean response = user.getResponseTo(event);
                if (response != null)
                    if (response.equals(true))
                        isOccupied = true;
            }
        }
        return isOccupied;
    }

    private Pair<String, String> id(String event, String promoter) {
        return new EventId(event, promoter);
    }
}
