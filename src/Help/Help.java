package Help;

public enum Help {
    register("registers a new account"),
    accounts("lists all registered accounts"),
    create("creates a new event"),
    events("lists all events of an account"),
    invite("invites an user to an event"),
    response("response to an invitation"),
    event("shows detailed information of an event"),
    topics("shows all events that cover a list of topics"),
    help("shows the available commands"),
    exit("terminates the execution of the program");

    final String msg;

    Help(String message) {
        msg = message;
    }

    public String toString() {
        return name() + " - " + msg;
    }
}
