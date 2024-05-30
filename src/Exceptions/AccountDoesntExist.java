package Exceptions;

/**
 * Exception thrown when an account doesn't exist in the system.
 */
public class AccountDoesntExist extends Exception {
    /**
     * Constructor to initialize the AccountAlreadyExists exception.
     * @param flag flag identifying the Account
     */
    public AccountDoesntExist(String flag) {
        super(flag);
    }
}
