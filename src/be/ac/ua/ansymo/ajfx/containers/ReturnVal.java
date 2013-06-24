package be.ac.ua.ansymo.ajfx.containers;

import java.util.List;

import soot.Type;
import soot.UnitPrinter;
import soot.Value;
import soot.util.Switch;

/**
 * Indicates the return value
 * @author Tim Molderez
 */
public class ReturnVal implements Value {

	public void apply(Switch sw) {
		// TODO Auto-generated method stub
	}

	public boolean equivTo(Object o) {
		// TODO Auto-generated method stub
		return false;
	}

	public int equivHashCode() {
		// TODO Auto-generated method stub
		return 0;
	}

	public List getUseBoxes() {
		// TODO Auto-generated method stub
		return null;
	}

	public Type getType() {
		// TODO Auto-generated method stub
		return null;
	}

	public void toString(UnitPrinter up) {
		// TODO Auto-generated method stub
		
	}
	
	public ReturnVal clone() {
		return new ReturnVal();
	}

}
