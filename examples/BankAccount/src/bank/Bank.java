package bank;

public class Bank {
	public Bank() {
		
	}
	
	public void start() {
		Account finn = new Account(250);
		Account jake = new Account(450);
		
		finn.transfer(jake, 100);
	}
}
