package Exceptions;

/**
 * Exception thrown when an account already exists in the system.
 */
public class AccountAlreadyExists extends Exception {

    /**
     * Constructor to initialize the AccountAlreadyExists exception.
     */
    public AccountAlreadyExists() {
        super("");
    }
}
