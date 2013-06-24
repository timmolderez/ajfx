package be.ac.ua.ansymo.ajfx.containers;

import java.util.List;

import soot.Local;
import soot.Type;
import soot.UnitPrinter;
import soot.util.Switch;

/**
 * Wrapper around a Local value.. (so that we can tell active locals from those out-of-scope..)
 * @author Tim Molderez
 */
public class DisabledLocal implements Local {
	Local local;
	
	public DisabledLocal(Local local) {
		this.local = local;
	}
	

	public void apply(Switch sw) {
		local.apply(sw);
	}

	public boolean equivTo(Object o) {
		return false;
	}

	public int equivHashCode() {
		return 0;
	}

	public List getUseBoxes() {
		return local.getUseBoxes();
	}

	public Type getType() {
		return local.getType();
	}

	public void toString(UnitPrinter up) {
		local.toString(up);
	}
	
	public Object clone() {
		return null;
	}

	public void setNumber(int number) {
		local.setNumber(number);
	}

	public int getNumber() {
		// TODO Auto-generated method stub
		return local.getNumber();
	}

	public String getName() {
		// TODO Auto-generated method stub
		return local.getName();
	}

	public void setName(String name) {
		local.setName(name);
	}

	public void setType(Type t) {
		local.setType(t);
	}

}
