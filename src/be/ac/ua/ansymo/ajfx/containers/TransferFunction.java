package be.ac.ua.ansymo.ajfx.containers;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import soot.SootMethod;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.IdentityStmt;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.ParameterRef;
import soot.jimple.Stmt;

/**
 * Contains all data to be passed between method bodies
 * Essentially a wrapper around a HashMap of VarTracker
 * @author Tim Molderez
 */
public class TransferFunction {
	Map<Value, VarTracker> vars;
	private VarTracker returnTracker = null;

	public TransferFunction() {
		vars = new HashMap<Value, VarTracker>();
	}
	
	public boolean contains(Value val) {
		return vars.containsKey(val);
	}
	
	/**
	 * Does this transferfunction contain a VarTracker that matches with (a part of) val?
	 * @param val
	 * @return
	 */
	public boolean containsPart(Value val) {
		return getFromPart(val)!=null;
	}
	
	public boolean containsPart(Stmt s) {
		return getFromPart(s)!=null;
	}
	
	public void add(Value val) {
		vars.put(val, new VarTracker(val));
	}
	
	public void add(Value val, VarTracker.Tag tag) {
		vars.put(val, new VarTracker(val, tag));
	}
	
	public void add(VarTracker vT) {
		vars.put(vT.var, vT);
	}
	
	public void addOrUpdate(VarTracker vT) {
		if (contains(vT.var)) {
			VarTracker existing = get(vT.var);
			existing.traces.putAll(vT.traces);
		} else {
			add(vT);
		}
	}
	
	public Collection<VarTracker> getVars() {
		return vars.values();
	}
	
	public VarTracker get(Value var) {
		return vars.get(var);
	}
	
	public VarTracker getFromPart(Value val) {
		if(contains(val)) {
			return vars.get(val);
		}
		
		List<Value> parts = val.getUseBoxes();
		for (Value part : parts) {
			if(contains(part)) {
				return vars.get(part);
			}
		}
		return null;
	}
	
	public VarTracker getFromPart(Stmt s) {
		List<ValueBox> parts = s.getUseBoxes();
		for (ValueBox part : parts) {
			if(contains(part.getValue())) {
				return vars.get(part.getValue());
			}
		}
		return null;
	}
	
	/**
	 * Add the data of tf into this TransferFunction
	 * @param tf	the results of processing a method call
	 * @param call	the call statement we just processed
	 */
	public void merge(TransferFunction tf, InvokeExpr call, SootMethod callee) {
		Collection<VarTracker> tfVars = tf.getVars();
		for (VarTracker var : tfVars) {
			// Everything global should be definitely added in
			if (var.isGlobal() && !var.isEmpty()) {
				addOrUpdate(var);
			
			// Parameters should be translated back to the calling context
			} else if (var.isParameter() && !var.isEmpty()) {
				// Do the inverse of Body.getParameterLocal(): Given a name, fetch the parameter's number
				Iterator<Unit> unitsIt = callee.getActiveBody().getUnits().iterator();
				int i=-1;
		        while (unitsIt.hasNext())
		        {
		            Unit s = unitsIt.next();
		            if (s instanceof IdentityStmt && ((IdentityStmt)s).getRightOp() instanceof ParameterRef)
		            {
		                IdentityStmt is = (IdentityStmt)s;
		                if (is.getLeftOp().equivTo(var)) {
		                	ParameterRef pr = (ParameterRef)is.getRightOp();
		                	i = pr.getIndex();
		                	break;
		                }
		            }
		        }
		        if (i!=-1) {
		        	VarTracker arg = getFromPart(call.getArg(i));
		        	arg.traces.putAll(var.traces);
		        }
		        
			// Local variables are copied over, but disabled so they can't interfere if we ever encounter another variable with the same name later on
			} else if (var.isLocal() && !var.isEmpty()) {
				var.disableLocal();
				add(var);
			
			// If we were tracking this, copy over the traces to the receiver in the calling context
			} else if (var.isThis() && !var.isEmpty()) {
				InstanceInvokeExpr icall = (InstanceInvokeExpr)call;
				get(icall.getBase()).traces.putAll(var.traces);
			}
		}
	}
	
	public void merge(TransferFunction tf) {
		Collection<VarTracker> tfVars = tf.getVars();
		for (VarTracker vT : tfVars) {
			if (!contains(vT.var)) {
				add(vT);
			} else {
				VarTracker vT2 = tf.get(vT.var);
				vT2.traces.putAll(vT.traces);
			}
		}
	}
	
	public String toString() {
		Collection<VarTracker> coll = vars.values();
		String result = "";
		for (VarTracker varTracker : coll) {
			result += varTracker.toString() + " - " + varTracker.traces.size() +  " defs\n";
		}
		return result;
	}
}