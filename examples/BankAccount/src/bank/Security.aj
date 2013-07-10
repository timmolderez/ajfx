package bank;

public aspect Security {
	int someField = 5;

	void around(Account to, int amount): call(void Account.transfer(Account, int)) && args(to, amount) {
//		System.out.println("Yo!");
//		this.someField = 5 + to.amount;
//		to.amount = 456;
		helperMethod();
		to.foo();
		
//		System.out.println(this.someField);
//		proceed(to, amount);
	}
	
	void helperMethod() {}
}
