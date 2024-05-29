import Exceptions.*;
import Help.*;
import Calendar.*;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Iterator;
import java.util.Scanner;

//dia 15 10:15
public class Main {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        String cmd;
        CalendarSystem system = new CalendarSystemClass();
        do {
            cmd = sc.next().toUpperCase();
            switch (cmd) {
                case "EVENT" -> event(sc.next(), sc.nextLine().trim(), system);
                case "TOPICS" -> topics(sc.nextLine().trim(), system);
                case "RESPONSE" -> response(sc.nextLine().trim(), sc.next(), sc.nextLine().trim(), sc.nextLine().trim(),
                        system);
                case "INVITE" -> invite(sc.next(), sc.next(), sc.nextLine().trim(), system);
                case "CREATE" -> create(sc.nextLine().trim(), sc.nextLine().trim(), sc.next(), sc.nextInt(),
                        sc.nextInt(), sc.nextInt(), Integer.parseInt(sc.nextLine().trim()), sc.nextLine().trim(),
                        system);
                case "ACCOUNTS" -> accounts(system);
                case "REGISTER" -> register(sc.next(), sc.next(), system);
                case "HELP" -> help();
                case "EVENTS" -> events(sc.next(), system);
                case "EXIT" -> System.out.println("Bye!");
                default -> System.out.printf("Unknown command %s. Type help to see available commands.\n", cmd);
            }
        } while (!cmd.equals("EXIT"));
    }

    private static void topics(String allTopics, CalendarSystem system) {
        //System.out.println("all topics is \"" + allTopics + "\"");
        String[] topics = allTopics.split("\\s+");
        //for (String topic : topics)
            //System.out.println("topic is \"" + topic + "\"");

        Iterator<Event> it = system.topics(topics);
        if (!it.hasNext()) {
            System.out.println("No events on those topics.");
            return;
        }

        System.out.print("Events on topics");
        for (String topic : topics) {
            System.out.printf(" %s", topic);
        }
        System.out.println(":");
        while (it.hasNext()) {
            Event event = it.next();
            Iterator<String> thisEventTopics = event.getTopics();
            System.out.printf("%s promoted by %s on", event.getName(), event.getPromoter());
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
            System.out.printf("%s occurs on %s:\n", event, DateTimeFormatter.ofPattern("dd-MM-yyyy HH'h'").format(date));
            while (invited.hasNext()) {
                String responseName;
                Boolean response = responses.next();
                if (response == null) responseName = "no_answer";
                else if (response.equals(true)) responseName = "accept";
                else responseName = "reject";
                System.out.printf("%s [%s]\n", invited.next(), responseName);
            }
        } catch (UserDoesntExist e) {
            System.out.printf("Account %s does not exist.\n", promoter);
        } catch (EventDoesntExist e) {
            System.out.printf("%s does not exist in account %s.\n", event, promoter);
        }
    }

    private static void response(String invited, String promoter, String eventName, String response, CalendarSystem system) {
        try {
            Iterator<Event> it = system.response(invited, promoter, eventName, response);
            System.out.printf("Account %s has replied %s to the invitation.\n", invited, response);
            while (it.hasNext()) {
                Event event = it.next();
                System.out.printf("%s promoted by %s was rejected.\n", event.getName(), event.getPromoter());
            }
        } catch (UserDoesntExist e) {
            switch (e.getMessage()) {
                case "p" -> System.out.printf("Account %s does not exist.\n", promoter);
                case "i" -> System.out.printf("Account %s does not exist.\n", invited);
                case "pi" -> {
                    System.out.printf("Account %s does not exist.\n", promoter);
                    System.out.printf("Account %s does not exist.\n", invited);
                }
            }
        } catch (InvalidResponce e) {
            System.out.println("Unknown event response.");
        } catch (EventDoesntExist e) {
            System.out.printf("%s does not exist in account %s.\n", eventName, promoter);
        } catch (NoInvitation e) {
            System.out.printf("Account %s is not on the invitation list.\n", invited);
        } catch (AlreadyRespondedToThatEvent e) {
            System.out.printf("Account %s has already responded.\n", invited);
        } catch (UserIsOcuppied e) {
            System.out.printf("%s already attending another event.\n", invited);
        }
    }


    private static void invite(String invited, String promoter, String eventName, CalendarSystem system) {
        try {
            Iterator<Event> it = system.invite(invited, promoter, eventName);
            if (!it.hasNext())
                System.out.printf("%s was invited.\n", invited);
            while (it.hasNext()) {
                Event event = it.next();
                if (event == null) {
                    System.out.printf("%s accepted the invitation.\n", invited);
                } else if (system.hasEvent(event.getName(), event.getPromoter())) {
                    System.out.printf("%s promoted by %s was rejected.\n", event.getName(), event.getPromoter());
                } else {
                    System.out.printf("%s promoted by %s was removed.\n", event.getName(), event.getPromoter());
                }
            }
        } catch (UserDoesntExist e) {
            switch (e.getMessage()) {
                case "p" -> System.out.printf("Account %s does not exist.\n", promoter);
                case "i" -> System.out.printf("Account %s does not exist.\n", invited);
                case "pi" -> {
                    System.out.printf("Account %s does not exist.\n", promoter);
                    System.out.printf("Account %s does not exist.\n", invited);
                }
            }
        } catch (EventDoesntExist e) {
            System.out.printf("%s does not exist in account %s.\n", eventName, promoter);
        } catch (AlreadyInvitedToThatEvent e) {
            System.out.printf("Account %s was already invited.\n", invited);
        } catch (HasAnotherEventAtThatTime e) {
            System.out.printf("Account %s already attending another event.\n", invited);
        }
    }

    private static void events(String accountName, CalendarSystem system) {
        try {
            Iterator<Pair<String, String>> events = system.getAccountEvents(accountName);
            if (!events.hasNext()) {
                System.out.printf("Account %s has no events.\n", accountName);
            } else {
                System.out.printf("Account %s events:\n", accountName);
            }
            while (events.hasNext()) {
                Pair<String, String> event = events.next();
                System.out.printf("%s status [invited %d] [accepted %d] [rejected %d] [unanswered %d]\n",
                        event.first(), system.getNOInvites(event.first(), event.second()), system.getNOAcceptedInvites(event.first(), event.second()),
                        system.getNORejectedInvites(event.first(), event.second()), system.getNOUnansweredInvites(event.first(), event.second()));
            }
        } catch (UserDoesntExist e) {
            System.out.printf("Account %s does not exist.\n", accountName);
        }
    }


    private static void create(String accountName, String eventName, String priorityName, int year, int month, int day, int hour, String topics, CalendarSystem system) {
        //System.out.println("account name = " + accountName);
        //System.out.println("event name = " + eventName);
        //System.out.println("topics = " + topics);
        LocalDateTime date = LocalDateTime.of(year, month, day, hour, 0, 0);
        try {
            system.create(accountName, eventName, priorityName, date, topics.split("\\s+"));
            System.out.printf("%s is scheduled.\n", eventName);
        } catch (UserDoesntExist e) {
            System.out.printf("Account %s does not exist.\n", accountName);
        } catch (UnknownPriority e) {
            System.out.println("Unknown priority type.");
        } catch (GuestsCantCreateEvents e) {
            System.out.printf("Guest account %s cannot create events.\n", accountName);
        } catch (StaffCantCreateHighPriorityEvents e) {
            System.out.printf("Account %s cannot create high priority events.\n", accountName);
        } catch (EventAlreadyExists e) {
            System.out.printf("%s already exists in account %s.\n", eventName, accountName);
        } catch (HasAnotherEventAtThatTime e) {
            System.out.printf("Account %s is busy.\n", accountName);
        }
    }

    private static void accounts(CalendarSystem system) {
        Iterator<Account> it = system.accounts();
        if (!it.hasNext())
            System.out.println("No account registered.");
        else
            System.out.println("All accounts:");
        while (it.hasNext()) {
            Account user = it.next();
            System.out.printf("%s [%s]\n", user.getName(), user.getType());
        }
    }

    private static void register(String name, String type, CalendarSystem system) {
        try {
            system.register(name, type.toUpperCase());
            System.out.printf("%s was registered.\n", name);
        } catch (UserAlreadyExists e) {
            System.out.printf("Account %s already exists.\n", name);
        } catch (InvalidType e) {
            System.out.println("Unknown account type.");
        }
    }

    private static void help() {
        Help[] helpMessages = Help.values();
        System.out.println("Available commands:");
        for (Help msg : helpMessages)
            System.out.println(msg);
    }
}