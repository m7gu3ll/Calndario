package Exceptions;

public class UserDoesntExist extends Exception {
    public UserDoesntExist(String userFlag) {
        super(userFlag);
    }
}
