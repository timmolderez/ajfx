package be.ac.ua.ansymo.ajfx.containers;

import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import soot.Local;
import soot.Value;
import soot.jimple.DefinitionStmt;
import soot.jimple.StaticFieldRef;

/**
 * This container class contains the necessary information to keep track of what happens to a single variable
 * A list of these VarTrackers essentially serves as the transfer function during the analysis.
 * @author Tim Molderez
 */
public class VarTracker {
	public Value var;
	
	public enum Tag{RETURN,PARAM,THIS};
	private Tag tag;
	
									/* Identifies the variable we're tracking
									 * Can represent different kinds of variables:
									 * Parameter	a number
									 * Local		a variable name
									 * Static		e.g. "Foo.bar"
									 * Instance		"this" */
	public VarTracker sourceVar;
	public DefinitionStmt sourceStmt;	/* If this variable is an alias of some other variable, source indicates the statement where the alias was created. */ 
	public Map<DefinitionStmt, Vector<Tree>> traces;
	
	/**
	 * Default constructor
	 */
	public VarTracker() {
		traces = new HashMap<DefinitionStmt, Vector<Tree>>();
	}
	
	public VarTracker(Value val) {
		this();
		var = val;
	}
	
	public VarTracker(Value val, Tag tag) {
		this(val);
		this.tag = tag;
	}
	
	public void addTrace(DefinitionStmt s, Tree node) {
		if (traces.containsKey(s)) {
			traces.get(s).add(node);
		} else {
			Vector<Tree> t = new Vector<Tree>();
			t.add(node);
			traces.put(s, t);
		}
	}
	
	/**
	 * If this VarTracker is tracking a local variable, we don't want it to interfere with other locals
	 * once it goes out-of-scope. Call this method to disable the local variable as soon as it goes out-of-scope.
	 */
	public void disableLocal() {
		var = new DisabledLocal((Local)var);
	}
	
	/**
	 * A VarTracker is empty if its value has never been changed
	 * @return
	 */
	public boolean isEmpty() {
		return traces.isEmpty();
	}
	
	public boolean isGlobal() {
		return var instanceof StaticFieldRef;
	}
	
	public boolean isParameter() {
//		return var instanceof ParameterRef;
		return tag==Tag.PARAM;
	}
	
	public boolean isLocal() {
		return var instanceof Local;
	}
	
	public boolean isThis() {
		return tag==Tag.THIS;
	}
	
	public String toString() {
		return var.toString();
	}
	
//	private Variable valueToVariable(Value val) {
//		if (val instanceof soot.Local) {
//			soot.Local l = (soot.Local)val;
//			Local v = new Local();
//			v.name = l.getName();
//			v.method = null;
//			return v;
//		} else if (val instanceof ParameterRef) {
//			ParameterRef p = (ParameterRef)val;
//			Parameter v = new Parameter();
//			v.number = p.getIndex();
//			return v;
//		} else if (val instanceof )
//	}
}
