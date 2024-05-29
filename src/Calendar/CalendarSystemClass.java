package Calendar;

import Exceptions.*;
import Utilities.Sort;

import java.time.LocalDateTime;
import java.util.*;

public class CalendarSystemClass implements CalendarSystem {
    public static final String STAFF = "STAFF";
    public static final String MANAGER = "MANAGER";
    public static final String GUEST = "GUEST";
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
            Event[] temp = eventsWithTopicsInCommon(topics, totalSize);
            for (int i = 0; i < temp.length; i++) {
                events.add(temp[i]);
            }
            totalSize--;
        }
        return events.iterator();
    }


    private Event[] eventsWithTopicsInCommon(String[] topics, int numberOfTopics) {
        List<Event> returnEvents = new ArrayList<>();

        Iterator<Event> it = events();
        while (it.hasNext()) {
            Event event = it.next();
            if (event.numberOfMatchingTopics(topics) == numberOfTopics) {
                returnEvents.add(event);
            }
        }

        Event[] events = returnEvents.toArray(new Event[0]);
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
        //System.out.println(user.getName() + " had " + event.getName() + " rejected. its response is " + user.getResponseTo(id(event.getName(), event.getPromoter())));
    }

    private List<Event> rejectAllThatConflictWith(Event event, Account user) {
        Iterator<Pair<String, String>> it = user.getEvents();
        List<Event> rejectedEvents = new ArrayList<>();
        while (it.hasNext()) {
            Event otherEvent = events.get(it.next());
            if (conflicts(otherEvent, event) &&
                    !Boolean.FALSE.equals(user.getResponseTo(id(otherEvent.getName(), otherEvent.getPromoter())))) {
                //System.out.printf("%s conflicts with %s\n", otherEvent.getName(), event.getName());
                //System.out.printf("%s response is %s\n", otherEvent.getName(), user.getResponseTo(id(event.getName(), event.getPromoter())));
                rejectInvite(otherEvent, user);
                //if (Boolean.TRUE.equals(user.getResponseTo(id(event.getName(), event.getPromoter()))))
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
            case "high":
                if (promoter instanceof Staff)
                    throw new StaffCantCreateHighPriorityEvents();
                priority++;
            case "mid":
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
        int usersMissing = 0;
        if (!hasUser(promoter))
            usersMissing++;
        if (!hasUser(invited))
            usersMissing += 2;
        switch (usersMissing) {
            case 1 -> throw new UserDoesntExist("p");
            case 2 -> throw new UserDoesntExist("i");
            case 3 -> throw new UserDoesntExist("pi");
        }

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
        List<Event> rejectedEvents = new ArrayList<>();
        if (event.getPriority() == 2 && user instanceof Staff) {
            rejectedEvents.add(null);
            Iterator<Pair<String, String>> it = user.getEvents();
            boolean foundHighPriorityEvent = false;
            Event canceledEvent = null;
            while (it.hasNext() && !foundHighPriorityEvent) {
                Pair<String, String> id = it.next();
                Event otherEvent = events.get(id(id.first(), id.second()));
                if (conflicts(otherEvent, event) &&
                        !Boolean.FALSE.equals(user.getResponseTo(id(otherEvent.getName(), otherEvent.getPromoter())))) {
                    //System.out.printf("e %s, priority %d\ne %s, priority %d\n", event.getName(), event.getPriority(), otherEvent.getName(), otherEvent.getPriority());
                    if (otherEvent.getPriority() == 2) {
                        rejectedEvents.set(0, event);
                        getInvited(event, user);
                        rejectInvite(event, user);
                        foundHighPriorityEvent = true;
                    } else if (otherEvent.getPromoter().equals(user.getName())) {
                        //System.out.printf("%s canceled\n", otherEvent.getName());
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
        } else {
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
            getInvited(event, user);
        }
        return rejectedEvents.iterator();
    }

    @Override
    public Iterator<Event> response(String invitedName, String promoterName, String eventName, String responseName) throws UserDoesntExist, InvalidResponce, NoInvitation, EventDoesntExist, AlreadyRespondedToThatEvent, UserIsOcuppied {
        int usersMissing = 0;
        boolean accepted;
        if (!hasUser(invitedName))
            usersMissing++;
        if (!hasUser(promoterName))
            usersMissing += 2;
        switch (usersMissing) {
            case 1 -> throw new UserDoesntExist("i");
            case 2 -> throw new UserDoesntExist("p");
            case 3 -> throw new UserDoesntExist("pi");
        }
        switch (responseName) {
            case "accept" -> accepted = true;
            case "reject" -> accepted = false;
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

        Iterator<Pair<String, String>> it = invited.getEvents();
        boolean foundEvent = false;
        while (it.hasNext() && !foundEvent) {
            Pair<String, String> id = it.next();
            Event otherEvent = events.get(id(id.first(), id.second()));
            if (conflicts(otherEvent, event)) {
                //System.out.printf("%s conflicts with %s\n", otherEvent.getName(), event.getName());
                //System.out.printf("%s response is %s\n", otherEvent.getName(), invited.getResponseTo(id));
                if (Boolean.TRUE.equals(invited.getResponseTo(id))) {
                    foundEvent = true;
                }
            }
        }
        if (foundEvent)
            throw new UserIsOcuppied();

        return acceptInvite(event, invited);
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
        while (it.hasNext()) {
            Pair<String, String> event = it.next();
            if (events.get(event).getDate().equals(date)) {
                Boolean response = user.getResponseTo(event);
                if (response != null)
                    if (response.equals(true))
                        return true;
            }
        }
        return false;
    }

    private Pair<String, String> id(String event, String promoter) {
        return new PairClass<>(event, promoter);
    }
}
