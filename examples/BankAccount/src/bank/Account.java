package bank;

public class Account {
	int amount;
	
	public Account(int m) {
		amount = m;
	}
	
	public void transfer(Account to, int m) {
		System.out.println("Transferring..");
		this.amount -= m;
		to.amount += m;
	}
	
	public void foo() {
		bar();
	}
	
	public void bar() {
		
	}
}
