import Exceptions.*;
import Help.*;
import Calendar.*;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Iterator;
import java.util.Scanner;
public class Main {

    public static final String EVENT = "EVENT";
    public static final String TOPICS = "TOPICS";
    public static final String RESPONSE = "RESPONSE";
    public static final String INVITE = "INVITE";
    public static final String CREATE = "CREATE";
    public static final String ACCOUNTS = "ACCOUNTS";
    public static final String REGISTER = "REGISTER";
    public static final String HELP = "HELP";
    public static final String EVENTS = "EVENTS";
    public static final String EXIT = "EXIT";
    public static final String BYE = "Bye!";
    public static final String NO_ANSWER = "no_answer";
    public static final String ACCEPT = "accept";
    public static final String REJECT = "reject";
    public static final String PROMOTER = "p";
    public static final String INVITEE = "i";
    public static final String PROMOTER_OR_INVITEE = "pi";
    public static final String UNKNOWN_COMMAND_S_TYPE_HELP_TO_SEE_AVAILABLE_COMMANDS = "Unknown command %s. Type help to see available commands.\n";
    public static final String NO_EVENTS_ON_THOSE_TOPICS = "No events on those topics.";
    public static final String EVENTS_ON_TOPICS = "Events on topics";
    public static final String S_PROMOTED_BY_S_ON = "%s promoted by %s on";
    public static final String S_OCCURS_ON_S = "%s occurs on %s:\n";
    public static final String EVENT_FORMAT = "%s [%s]\n";
    public static final String ACCOUNT_S_DOES_NOT_EXIST = "Account %s does not exist.\n";
    public static final String S_DOES_NOT_EXIST_IN_ACCOUNT_S = "%s does not exist in account %s.\n";
    public static final String ACCOUNT_S_HAS_REPLIED_S_TO_THE_INVITATION = "Account %s has replied %s to the invitation.\n";
    public static final String S_PROMOTED_BY_S_WAS_REJECTED = "%s promoted by %s was rejected.\n";
    public static final String UNKNOWN_EVENT_RESPONSE = "Unknown event response.";
    public static final String ACCOUNT_S_IS_NOT_ON_THE_INVITATION_LIST = "Account %s is not on the invitation list.\n";
    public static final String ACCOUNT_S_HAS_ALREADY_RESPONDED = "Account %s has already responded.\n";
    public static final String S_ALREADY_ATTENDING_ANOTHER_EVENT = "%s already attending another event.\n";
    public static final String S_WAS_INVITED = "%s was invited.\n";
    public static final String S_ACCEPTED_THE_INVITATION = "%s accepted the invitation.\n";
    public static final String S_PROMOTED_BY_S_WAS_REMOVED = "%s promoted by %s was removed.\n";
    public static final String ACCOUNT_S_WAS_ALREADY_INVITED = "Account %s was already invited.\n";
    public static final String ACCOUNT_S_HAS_NO_EVENTS = "Account %s has no events.\n";
    public static final String ACCOUNT_S_EVENTS = "Account %s events:\n";
    public static final String EVENTS_FORMAT = "%s status [invited %d] [accepted %d] [rejected %d] [unanswered %d]\n";
    public static final String S_IS_SCHEDULED = "%s is scheduled.\n";
    public static final String UNKNOWN_PRIORITY_TYPE = "Unknown priority type.";
    public static final String GUEST_ACCOUNT_S_CANNOT_CREATE_EVENTS = "Guest account %s cannot create events.\n";
    public static final String ACCOUNT_S_CANNOT_CREATE_HIGH_PRIORITY_EVENTS = "Account %s cannot create high priority events.\n";
    public static final String S_ALREADY_EXISTS_IN_ACCOUNT_S = "%s already exists in account %s.\n";
    public static final String ACCOUNT_S_IS_BUSY = "Account %s is busy.\n";
    public static final String NO_ACCOUNT_REGISTERED = "No account registered.";
    public static final String ALL_ACCOUNTS = "All accounts:";
    public static final String S_WAS_REGISTERED = "%s was registered.\n";
    public static final String ACCOUNT_S_ALREADY_EXISTS = "Account %s already exists.\n";
    public static final String UNKNOWN_ACCOUNT_TYPE = "Unknown account type.";
    public static final String AVAILABLE_COMMANDS = "Available commands:";
    public static final String ACCOUNT_S_ALREADY_ATTENDING_ANOTHER_EVENT = "Account %s already attending another event.\n";

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        String cmd;
        CalendarSystem system = new CalendarSystemClass();
        do {
            cmd = sc.next().toUpperCase();
            switch (cmd) {
                case EVENT -> event(sc.next(), sc.nextLine().trim(), system);
                case TOPICS -> topics(sc.nextLine().trim(), system);
                case RESPONSE -> response(sc.nextLine().trim(), sc.next(), sc.nextLine().trim(), sc.nextLine().trim(),
                        system);
                case INVITE -> invite(sc.next(), sc.next(), sc.nextLine().trim(), system);
                case CREATE -> create(sc.nextLine().trim(), sc.nextLine().trim(), sc.next(), sc.nextInt(),
                        sc.nextInt(), sc.nextInt(), Integer.parseInt(sc.nextLine().trim()), sc.nextLine().trim(),
                        system);
                case ACCOUNTS -> accounts(system);
                case REGISTER -> register(sc.next(), sc.next(), system);
                case HELP -> help();
                case EVENTS -> events(sc.next(), system);
                case EXIT -> System.out.println(BYE);
                default -> System.out.printf(UNKNOWN_COMMAND_S_TYPE_HELP_TO_SEE_AVAILABLE_COMMANDS, cmd);
            }
        } while (!cmd.equals(EXIT));
        sc.close();
    }

    private static void topics(String allTopics, CalendarSystem system) {
        String[] topics = allTopics.split("\\s+");
        Iterator<Event> it = system.topics(topics);
        if (!it.hasNext()) {
            System.out.println(NO_EVENTS_ON_THOSE_TOPICS);
            return;
        }

        System.out.print(EVENTS_ON_TOPICS);
        for (String topic : topics) {
            System.out.printf(" %s", topic);
        }
        System.out.println(":");
        while (it.hasNext()) {
            Event event = it.next();
            Iterator<String> thisEventTopics = event.getTopics();
            System.out.printf(S_PROMOTED_BY_S_ON, event.getName(), event.getPromoter());
            while (thisEventTopics.hasNext())
                System.out.printf(" %s", thisEventTopics.next());
            System.out.println();
        }
    }

    private static void event(String promoter, String event, CalendarSystem system) {
        try {
            LocalDateTime date = system.getEventDate(event, promoter);
            Iterator<String> invited = system.getEventInvited(event, promoter);
            Iterator<Boolean> responses = system.getEventResponses(event, promoter);
            System.out.printf(S_OCCURS_ON_S, event, DateTimeFormatter.ofPattern("dd-MM-yyyy HH'h'").format(date));
            while (invited.hasNext()) {
                String responseName;
                Boolean response = responses.next();
                if (response == null) responseName = NO_ANSWER;
                else if (response.equals(true)) responseName = ACCEPT;
                else responseName = REJECT;
                System.out.printf(EVENT_FORMAT, invited.next(), responseName);
            }
        } catch (UserDoesntExist e) {
            System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, promoter);
        } catch (EventDoesntExist e) {
            System.out.printf(S_DOES_NOT_EXIST_IN_ACCOUNT_S, event, promoter);
        }
    }

    private static void response(String invited, String promoter, String eventName, String response, CalendarSystem system) {
        try {
            Iterator<Event> it = system.response(invited, promoter, eventName, response);
            System.out.printf(ACCOUNT_S_HAS_REPLIED_S_TO_THE_INVITATION, invited, response);
            while (it.hasNext()) {
                Event event = it.next();
                System.out.printf(S_PROMOTED_BY_S_WAS_REJECTED, event.getName(), event.getPromoter());
            }
        } catch (UserDoesntExist e) {
            printUserDoesntExist(invited, promoter, e);
        } catch (InvalidResponce e) {
            System.out.println(UNKNOWN_EVENT_RESPONSE);
        } catch (EventDoesntExist e) {
            System.out.printf(S_DOES_NOT_EXIST_IN_ACCOUNT_S, eventName, promoter);
        } catch (NoInvitation e) {
            System.out.printf(ACCOUNT_S_IS_NOT_ON_THE_INVITATION_LIST, invited);
        } catch (AlreadyRespondedToThatEvent e) {
            System.out.printf(ACCOUNT_S_HAS_ALREADY_RESPONDED, invited);
        } catch (HasAnotherEventAtThatTime e) {
            System.out.printf(S_ALREADY_ATTENDING_ANOTHER_EVENT, invited);
        }
    }


    private static void invite(String invited, String promoter, String eventName, CalendarSystem system) {
        try {
            Iterator<Event> it = system.invite(invited, promoter, eventName);
            if (!it.hasNext())
                System.out.printf(S_WAS_INVITED, invited);
            while (it.hasNext()) {
                Event event = it.next();
                if (event == null) {
                    System.out.printf(S_ACCEPTED_THE_INVITATION, invited);
                } else if (system.hasEvent(event.getName(), event.getPromoter())) {
                    System.out.printf(S_PROMOTED_BY_S_WAS_REJECTED, event.getName(), event.getPromoter());
                } else {
                    System.out.printf(S_PROMOTED_BY_S_WAS_REMOVED, event.getName(), event.getPromoter());
                }
            }
        } catch (UserDoesntExist e) {
            printUserDoesntExist(invited, promoter, e);
        } catch (EventDoesntExist e) {
            System.out.printf(S_DOES_NOT_EXIST_IN_ACCOUNT_S, eventName, promoter);
        } catch (AlreadyInvitedToThatEvent e) {
            System.out.printf(ACCOUNT_S_WAS_ALREADY_INVITED, invited);
        } catch (HasAnotherEventAtThatTime e) {
            System.out.printf(ACCOUNT_S_ALREADY_ATTENDING_ANOTHER_EVENT, invited);
        }
    }

    private static void printUserDoesntExist(String invited, String promoter, UserDoesntExist e) {
        switch (e.getMessage()) {
            case PROMOTER -> System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, promoter);
            case INVITEE -> System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, invited);
            case PROMOTER_OR_INVITEE -> {
                System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, promoter);
                System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, invited);
            }
        }
    }

    private static void events(String accountName, CalendarSystem system) {
        try {
            Iterator<Pair<String, String>> events = system.getAccountEvents(accountName);
            if (!events.hasNext()) {
                System.out.printf(ACCOUNT_S_HAS_NO_EVENTS, accountName);
            } else {
                System.out.printf(ACCOUNT_S_EVENTS, accountName);
            }
            while (events.hasNext()) {
                Pair<String, String> event = events.next();
                System.out.printf(EVENTS_FORMAT,
                        event.first(), system.getNOInvites(event.first(), event.second()), system.getNOAcceptedInvites(event.first(), event.second()),
                        system.getNORejectedInvites(event.first(), event.second()), system.getNOUnansweredInvites(event.first(), event.second()));
            }
        } catch (UserDoesntExist e) {
            System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, accountName);
        }
    }


    private static void create(String accountName, String eventName, String priorityName, int year, int month, int day, int hour, String topics, CalendarSystem system) {
        LocalDateTime date = LocalDateTime.of(year, month, day, hour, 0, 0);
        try {
            system.create(accountName, eventName, priorityName, date, topics.split("\\s+"));
            System.out.printf(S_IS_SCHEDULED, eventName);
        } catch (UserDoesntExist e) {
            System.out.printf(ACCOUNT_S_DOES_NOT_EXIST, accountName);
        } catch (UnknownPriority e) {
            System.out.println(UNKNOWN_PRIORITY_TYPE);
        } catch (GuestsCantCreateEvents e) {
            System.out.printf(GUEST_ACCOUNT_S_CANNOT_CREATE_EVENTS, accountName);
        } catch (StaffCantCreateHighPriorityEvents e) {
            System.out.printf(ACCOUNT_S_CANNOT_CREATE_HIGH_PRIORITY_EVENTS, accountName);
        } catch (EventAlreadyExists e) {
            System.out.printf(S_ALREADY_EXISTS_IN_ACCOUNT_S, eventName, accountName);
        } catch (HasAnotherEventAtThatTime e) {
            System.out.printf(ACCOUNT_S_IS_BUSY, accountName);
        }
    }

    private static void accounts(CalendarSystem system) {
        Iterator<Account> it = system.accounts();
        if (!it.hasNext())
            System.out.println(NO_ACCOUNT_REGISTERED);
        else
            System.out.println(ALL_ACCOUNTS);
        while (it.hasNext()) {
            Account user = it.next();
            System.out.printf(EVENT_FORMAT, user.getName(), user.getType());
        }
    }

    private static void register(String name, String type, CalendarSystem system) {
        try {
            system.register(name, type.toUpperCase());
            System.out.printf(S_WAS_REGISTERED, name);
        } catch (UserAlreadyExists e) {
            System.out.printf(ACCOUNT_S_ALREADY_EXISTS, name);
        } catch (InvalidType e) {
            System.out.println(UNKNOWN_ACCOUNT_TYPE);
        }
    }

    private static void help() {
        Help[] helpMessages = Help.values();
        System.out.println(AVAILABLE_COMMANDS);
        for (Help msg : helpMessages)
            System.out.println(msg);
    }
}