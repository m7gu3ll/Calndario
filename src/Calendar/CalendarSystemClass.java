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
    Map<String, Account> accounts;
    Map<Pair<String, String>, Event> events;

    public CalendarSystemClass() {
        accounts = new TreeMap<>();
        events = new TreeMap<>();
    }
    @Override
    public boolean hasAccount(String name) {
        return accounts.containsKey(name);
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
    public void register(String name, String type) throws AccountAlreadyExists, InvalidType {
        if (hasAccount(name))
            throw new AccountAlreadyExists();
        if (typeIsInvalid(type))
            throw new InvalidType();

        Account newAccount = null;
        switch (type) {
            case GUEST -> newAccount = new Guest(name);
            case STAFF -> newAccount = new Staff(name);
            case MANAGER -> newAccount = new Manager(name);
        }
        accounts.put(name, newAccount);
    }

    @Override
    public Iterator<Account> accounts() {
        return accounts.values().iterator();
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
                    !Boolean.FALSE.equals(account.getResponseTo(id(otherEvent.getName(), otherEvent.getPromoter())))) {
                rejectInvite(otherEvent, account);
                rejectedEvents.add(otherEvent);
            }
        }
        return rejectedEvents;
    }

    private void cancel(Event event) {
        Iterator<String> it = event.getInvited();
        while (it.hasNext()) {
            accounts.get(it.next()).remove(id(event.getName(), event.getPromoter()));
        }
        events.remove(id(event.getName(), event.getPromoter()));
    }

    private void promote(Pair<String, String> id) {
        Event event = events.get(id);
        Account account = accounts.get(event.getPromoter());
        rejectAllThatConflictWith(event, account);
        account.promote(id);
    }

    @Override
    public void create(String accountName, String eventName, String priorityName, LocalDateTime date,
                       String[] topics) throws AccountDoesntExist, UnknownPriority,
            GuestsCantCreateEvents, StaffCantCreateHighPriorityEvents, EventAlreadyExists,
            HasAnotherEventAtThatTime {
        Pair<String, String> id = id(eventName, accountName);
        if (!hasAccount(accountName))
            throw new AccountDoesntExist(null);
        Account promoter = accounts.get(accountName);
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
        if (accountIsOccupied(accountName, date))
            throw new HasAnotherEventAtThatTime();

        events.put(id, new EventClass(eventName, accountName, priority, date, topics));
        promote(id);
    }

    @Override
    public Iterator<Event> invite(String invited, String promoter, String eventName) throws AccountDoesntExist, EventDoesntExist, AlreadyInvitedToThatEvent, HasAnotherEventAtThatTime {
        checkIfAccountsExist(invited, promoter);

        Event event = events.get(id(eventName, promoter));
        Account account = accounts.get(invited);
        if (event == null)
            throw new EventDoesntExist();
        if (!event.getPromoter().equals(promoter))
            throw new EventDoesntExist();
        if (account == null)
            throw new AlreadyInvitedToThatEvent();
        if (account.wasInvitedTo(id(eventName, promoter)))
            throw new AlreadyInvitedToThatEvent();

        return invite(account, event);
    }

    private Iterator<Event> invite(Account user, Event event) throws HasAnotherEventAtThatTime {
        if (event.getPriority() == 2 && user instanceof Staff) {
            return getInvitedToHighPriorityEvent(user, event);
        } else {
            checkForEventsAtTheSameTime(event, account);
            getInvited(event, account);
        }
        return noEvents();
    }

    private Iterator<Event> getInvitedToHighPriorityEvent(Account account, Event event) throws HasAnotherEventAtThatTime {
        List<Event> rejectedEvents = new ArrayList<>();
        rejectedEvents.add(null);
        Iterator<Pair<String, String>> it = account.getEvents();
        boolean foundHighPriorityEvent = false;
        Event canceledEvent = null;
        while (it.hasNext() && !foundHighPriorityEvent) {
            Pair<String, String> id = it.next();
            Event otherEvent = events.get(id(id.first(), id.second()));
            if (conflicts(otherEvent, event) &&
                    !Boolean.FALSE.equals(account.getResponseTo(id(otherEvent.getName(), otherEvent.getPromoter())))) {
                if (otherEvent.getPriority() == 2) {
                    rejectedEvents.set(0, event);
                    getInvited(event, account);
                    rejectInvite(event, account);
                    foundHighPriorityEvent = true;
                } else if (otherEvent.getPromoter().equals(account.getName())) {
                    rejectedEvents.add(otherEvent);
                    canceledEvent = otherEvent;
                } else {
                    rejectedEvents.add(otherEvent);
                    rejectInvite(otherEvent, account);
                }
            }
        }
        if (!foundHighPriorityEvent) {
            getInvited(event, account);
            acceptInvite(event, account);
        } else {
            throw new HasAnotherEventAtThatTime();
        }
        if (canceledEvent != null)
            cancel(canceledEvent);
        return rejectedEvents.iterator();
    }

    private void checkForEventsAtTheSameTime(Event event, Account account) throws HasAnotherEventAtThatTime {
        Iterator<Pair<String, String>> it = account.getEvents();
        boolean foundEvent = false;
        while (it.hasNext() && !foundEvent) {
            Pair<String, String> id = it.next();
            Event otherEvent = events.get(id(id.first(), id.second()));
            if (conflicts(otherEvent, event))
                if (Boolean.TRUE.equals(account.getResponseTo(id)))
                    foundEvent = true;
        }
        if (foundEvent)
            throw new HasAnotherEventAtThatTime();
    }

    @Override
    public Iterator<Event> response(String invitedName, String promoterName, String eventName, String responseName) throws AccountDoesntExist, InvalidResponse, NoInvitation, EventDoesntExist, AlreadyRespondedToThatEvent, HasAnotherEventAtThatTime {
        boolean accepted;
        checkIfAccountsExist(invitedName, promoterName);
        switch (responseName) {
            case ACCEPT -> accepted = true;
            case REJECT -> accepted = false;
            default -> throw new InvalidResponse();
        }
        Account invited = accounts.get(invitedName);
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

    private void checkIfAccountsExist(String invitedName, String promoterName) throws AccountDoesntExist {
        int usersMissing = 0;
        if (!hasAccount(promoterName))
            usersMissing++;
        if (!hasAccount(invitedName))
            usersMissing += 2;
        switch (usersMissing) {
            case PROMOTER_MISSING -> throw new AccountDoesntExist("p");
            case INVITEE_MISSING -> throw new AccountDoesntExist("i");
            case PROMOTER_AND_INVITEE_MISSING -> throw new AccountDoesntExist("pi");
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
    public LocalDateTime getEventDate(String event, String promoter) throws AccountDoesntExist, EventDoesntExist {
        if (accounts.get(promoter) == null)
            throw new AccountDoesntExist(null);
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
    public Iterator<Pair<String, String>> getAccountEvents(String account) throws AccountDoesntExist {
        if (!hasAccount(account))
            throw new AccountDoesntExist(null);

        return accounts.get(account).getEvents();
    }

    @Override
    public boolean accountIsOccupied(String accountName, LocalDateTime date) {
        Account account = accounts.get(accountName);
        Iterator<Pair<String, String>> it = account.getEvents();
        boolean isOccupied = false;
        while (it.hasNext() && !isOccupied) {
            Pair<String, String> event = it.next();
            if (events.get(event).getDate().equals(date)) {
                Boolean response = account.getResponseTo(event);
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
