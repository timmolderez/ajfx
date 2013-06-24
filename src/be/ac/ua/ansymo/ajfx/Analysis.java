package be.ac.ua.ansymo.ajfx;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import soot.Local;
import soot.Scene;
import soot.SootMethod;
import soot.Unit;
import soot.Value;
import soot.ValueBox;
import soot.jimple.DefinitionStmt;
import soot.jimple.InstanceInvokeExpr;
import soot.jimple.InvokeExpr;
import soot.jimple.ReturnStmt;
import soot.jimple.StaticFieldRef;
import soot.jimple.Stmt;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.Edge;
import soot.toolkits.graph.BriefUnitGraph;
import soot.util.dot.DotGraph;
import abc.da.weaving.weaver.depadviceopt.ds.WeavableMethods;
import abc.main.Main;
import abc.weaving.aspectinfo.AbstractAdviceDecl;
import abc.weaving.aspectinfo.AdviceDecl;
import abc.weaving.aspectinfo.AdviceSpec;
import abc.weaving.aspectinfo.AfterAdvice;
import abc.weaving.aspectinfo.AfterReturningAdvice;
import abc.weaving.aspectinfo.AfterThrowingAdvice;
import abc.weaving.aspectinfo.AndPointcut;
import abc.weaving.aspectinfo.ArgVar;
import abc.weaving.aspectinfo.Args;
import abc.weaving.aspectinfo.AroundAdvice;
import abc.weaving.aspectinfo.BeforeAdvice;
import abc.weaving.aspectinfo.BeforeAfterAdvice;
import abc.weaving.aspectinfo.Formal;
import abc.weaving.aspectinfo.GlobalAspectInfo;
import abc.weaving.aspectinfo.OrPointcut;
import abc.weaving.aspectinfo.Pointcut;
import abc.weaving.aspectinfo.Var;
import abc.weaving.matching.AdviceApplication;
import abc.weaving.matching.AdviceFormals;
import abc.weaving.matching.BodyShadowMatch;
import abc.weaving.matching.MethodAdviceList;
import abc.weaving.matching.ShadowMatch;
import abc.weaving.matching.StmtShadowMatch;
import abc.weaving.residues.AdviceFormal;
import abc.weaving.residues.JimpleValue;
import abc.weaving.weaver.ReweavingAnalysis;
import be.ac.ua.ansymo.ajfx.containers.DefUsePair;
import be.ac.ua.ansymo.ajfx.containers.TransferFunction;
import be.ac.ua.ansymo.ajfx.containers.Tree;
import be.ac.ua.ansymo.ajfx.containers.VarTracker;
import be.ac.ua.ansymo.ajfx.util.LogFormatter;
import be.ac.ua.ansymo.ajfx.util.SootUtils;
import be.ac.ua.ansymo.ajfx.util.Timer;

/**
 * ajfx analysis
 * @author Tim Molderez
 */
public class Analysis implements ReweavingAnalysis {
	// Crosscutting logging & profiling stuff..
	private static Logger log;
	private static Timer timer;

	private CallGraph cg; // Call graph
//	private HashMap<AbstractAdviceDecl, HashSet<String>> allPackageCaches; // Package cache

	/**
	 * Top-level method performing ajfx's analysis, which is to find all fields modified during the execution of an advice, and
	 * to find what is directly affected by these modifications, i.e. where is that data first used.
	 */
	public boolean analyze() {
		setupAnalysis(); // Set up logging/timing stuff..
		
		buildCallGraph();
		SootUtils util = new SootUtils(cg);
		DotGraph dot = util.exportCallGraphToDot("call graph");
		dot.plot("callgraph.dot");
		
		debug("Call graph constructed in " + timer.getTimePassed() + "ms");

		// Analyse each advice separately
		GlobalAspectInfo aspectInfo = Main.v().getAbcExtension().getGlobalAspectInfo();
		Map<AbstractAdviceDecl, DefUsePair> results = new HashMap<AbstractAdviceDecl, DefUsePair>();
		
		for (Iterator<AbstractAdviceDecl> adDeclIt = aspectInfo.getAdviceDecls().iterator(); adDeclIt.hasNext(); ) {
			AbstractAdviceDecl aadecl = adDeclIt.next();
			debug(">>> Started analysing " + aadecl);
			
//			allPackageCaches.put(aadDecl, new HashSet<String>());
			if (aadecl instanceof AdviceDecl) {
				SootMethod m = ((AdviceDecl)aadecl).getImpl().getSootMethod();
				DefUsePair du = new DefUsePair();
				results.put(aadecl, du);
				
				// First phase of the analysis: For each advice, go find all relevant definitions in all potential control flows. 
				du.defs = findDefs(m);
				debug("Finished finding definitions in " + timer.getTimePassed() + " ms");
				
				// Second phase of the analysis: Given the definitions we found, now look for all uses of them.
				du.uses = findUses(aadecl, du.defs);
				debug("Finished finding uses in " + timer.getTimePassed() + " ms");
			}
		}
		

		return false;
	}
	
	/*
	 * Construct the call graph, covering only what is reachable from advice
	 */
	private void buildCallGraph() {
		// Construct the call graph, with all advice and their joinpoint shadows as the entry points
		AdviceCallGraphBuilder cgb = new AdviceCallGraphBuilder();
//		CallGraphBuilder cgb = new CallGraphBuilder();
		cgb.build();
		this.cg = Scene.v().getCallGraph();
	}

	/*
	 *  First phase of the analysis - go find all interesting definitions reachable from an advice body
	 */
	private TransferFunction findDefs(SootMethod m) {
		// Set up data structures
		TransferFunction defs = new TransferFunction();
		Tree<Object> traceTree = new Tree<Object>(m);
		
		// Initial tracking sets: Track what happens to all of the parameters bound by the advice
		for(int i=0; i<m.getParameterCount(); i++) {
//			defs.put(new Integer(i).toString());
//			ParameterRef p = new ParameterRef(m.getParameterType(i), i);
			defs.add(new VarTracker(m.getActiveBody().getParameterLocal(i)));
		}
		
		return findDefs_recursive(m, defs, traceTree);
		
		
		
		// Package cache for this advice
//		HashSet<String> packageCache = new HashSet<String>();
//		allPackageCaches.put(adDecl, advicePackageCache);
	}
	
	private TransferFunction findDefs_recursive(SootMethod m, TransferFunction tf, Tree caller) {
		// Stop if we can't access the body for some reason (e.g. in case of library calls)
		if (!m.hasActiveBody() || m.getName().equals("<clinit>")) {
			return tf;
		}
		
		// Traverse each unit/stmt in this body
		Iterator<Unit> gIt = m.getActiveBody().getUnits().iterator();
		while(gIt.hasNext()) {
			Stmt s = (Stmt)gIt.next();
			
			// If a method call
			if (s.containsInvokeExpr()) {
				// Get all potential lookup results (naive/simple: We assume it could be any subtype of the static type..)
				Iterator<Edge> mBodies = cg.edgesOutOf(s);
				while (mBodies.hasNext()) {
					Edge edge = (Edge) mBodies.next();
					SootMethod callee = (SootMethod)edge.getTgt();

					Tree child = caller.addChild(s);
					TransferFunction in = constructInputDefTf(s.getInvokeExpr(), callee, tf);
					TransferFunction result = findDefs_recursive(callee, in, child);
					tf.merge(result, s.getInvokeExpr(), callee);
				}
			}

			// If a definition/assignment
			if (s instanceof DefinitionStmt) {
				// If the RHS contains an invocation, and we should track 
				
				DefinitionStmt ds = (DefinitionStmt)s;
				processDefStmt(tf, ds, caller); // Update out to take into account ds
			}

			
			
			// In case of a return statement
			if (s instanceof ReturnStmt) {
				ReturnStmt r = (ReturnStmt)s;
				if (tf.containsPart(s)){
					tf.add(r.getOp(), VarTracker.Tag.RETURN);
				}
			}
		}

		return tf;
	}
	
	/*
	 * Find all relevant uses of a certain advice
	 * @param aadl
	 * @return		a map which maps the advice's join point shadows to their transfer function
	 */
	private Map<ShadowMatch,TransferFunction> findUses(AbstractAdviceDecl aadl, TransferFunction defs) {
		Map<ShadowMatch,TransferFunction> results = new HashMap<ShadowMatch, TransferFunction>();
		
		AdviceSpec aspec = aadl.getAdviceSpec();
		
		// Analyse each join point shadow seperately
		List<ShadowMatch> shadows = getJoinPointShadows(aadl);
		for (ShadowMatch shadow : shadows) {
			// In case of execution join points, analyse the call sites that may execute this method body
			if (shadow instanceof BodyShadowMatch) {
				BodyShadowMatch bShad = (BodyShadowMatch) shadow;
				
				SootMethod callee = bShad.getContainer();
				Iterator<Edge> callers = cg.edgesInto(callee);
				while(callers.hasNext()) {
					SootMethod caller = (SootMethod) callers.next().getSrc();
					List<Stmt> callSites = getCallSites(caller, callee);
					TransferFunction out = new TransferFunction();
					for (Stmt stmt : callSites) {
						TransferFunction in = constructInputUseTf(defs, aadl, shadow, stmt.getInvokeExpr());
						out.merge(findUses_recursive(stmt, caller, in));
					}
				}
			
			// In case of call join points, analyse the call site 
			} else if (shadow instanceof StmtShadowMatch) {
				StmtShadowMatch sShad = (StmtShadowMatch) shadow;
				Stmt s = sShad.getStmt();
				
				
				if (isAfterAdvice(aspec) || isAroundAdvice(aspec)) {
					s = getNextStmt(s, sShad.getContainer());
				}
				
			}
		}
		
		return results;
	}	
	
	private TransferFunction findUses_recursive(Stmt s, SootMethod m, TransferFunction in) {
		return new TransferFunction();
	}

	private Args getArgsFromPointcut(Pointcut pcut) {
		if (pcut instanceof AndPointcut) {
			AndPointcut apcut = (AndPointcut) pcut;
			Args l = getArgsFromPointcut(apcut.getLeftPointcut());
			if (l!=null) {
				return l;
			}
			return getArgsFromPointcut(apcut.getRightPointcut());
		} else if (pcut instanceof OrPointcut) {
			OrPointcut opcut = (OrPointcut) pcut;
			Args l = getArgsFromPointcut(opcut.getLeftPointcut());
			if (l!=null) {
				return l;
			}
			return getArgsFromPointcut(opcut.getRightPointcut());
		} else if (pcut instanceof Args) {
			return (Args) pcut;
		} else {
			return null;
		}
	}
	
	/*
	 * Return the statement that comes right after a particular statement 
	 * @param s		a statement
	 * @param m		the method that contains s
	 * @return		the next statement (null if there is none)
	 */
	private Stmt getNextStmt(Stmt s, SootMethod m) {
		BriefUnitGraph g = new BriefUnitGraph(m.getActiveBody());
		Iterator<Unit> gIt = g.iterator();
		while(gIt.hasNext()) {
			if (gIt.next().equals(s)) {
				if (gIt.hasNext()) {
					return (Stmt) gIt.next();
				}
			}
		}
		
		return null;
	}
	
	/*
	 * Given a method body, go find all calls to a particular method
	 */
	private List<Stmt> getCallSites(SootMethod caller, SootMethod callee) {
		List<Stmt> callSites = new Vector<Stmt>();
		
		BriefUnitGraph g = new BriefUnitGraph(caller.getActiveBody());
		Iterator<Unit> gIt = g.iterator();
		while(gIt.hasNext()) {
			Stmt s = (Stmt)gIt.next();
			if (s.containsInvokeExpr()) {
				SootMethod m = s.getInvokeExpr().getMethod();
				if (m.getSignature().equals(callee.getSignature())) {
					callSites.add(s);
				}
			}
		}
		
		return callSites;
	} 
	
	
	
	/*
	 * Construct an initial TransferFunction to be used as input when processing the body of a called method 
	 * @param caller
	 * @param callee	
	 * @param in		VarTracker vector in the context of the caller
	 * @return
	 */
	private TransferFunction constructInputDefTf(InvokeExpr call, SootMethod callee, TransferFunction in) {
		TransferFunction out = new TransferFunction();
		
		// If we're tracking the call's receiver, the callee should track the this pointer
		if (call instanceof InstanceInvokeExpr) {
			InstanceInvokeExpr icall = (InstanceInvokeExpr)call;
			if (in.contains(icall.getBase())) {
				out.add(callee.getActiveBody().getThisLocal(), VarTracker.Tag.THIS);
			}
		}
		
		// Which of the actual arguments contains stuff we're tracking?
		List<Value> args = call.getArgs();
		try {
		for(int i=0; i<args.size(); i++) {
			// Does the actual argument (or part of it) match with an element in the transfer function?
			if (in.containsPart(args.get(i))) {
				Local par = callee.getActiveBody().getParameterLocal(i);
				out.add(par, VarTracker.Tag.PARAM);
			}
		}
		} catch (RuntimeException e) {}
		
		return out;
	}
	
	/*
	 * Construct an initial Transferfunction to be used when looking for uses in an advice body 
	 * @param defsOutput
	 * @param aadecl
	 * @param call
	 * @return
	 */
	private TransferFunction constructInputUseTf(TransferFunction defsOutput, AbstractAdviceDecl aadecl, ShadowMatch shadow, InvokeExpr call) {
		TransferFunction result = new TransferFunction();
		Collection<VarTracker> vars = defsOutput.getVars();
		Args args = getArgsFromPointcut(aadecl.getPointcut());
		List<ArgVar> allArgs = new Vector<ArgVar>();
		if (args!= null) {
			allArgs = args.getArgs();
		}
		
		for (VarTracker vT : vars) {
			// If this tracks something global, just add an entry for it  
			if (vT.var instanceof StaticFieldRef) {
				result.add(vT.var);
				
			// Otherwise, see if you can map it to something bound by args()
			} else if (vT.var instanceof Local) {
				int i=0;
				for (ArgVar arg : allArgs) {
					if (vT.var.toString().equals(arg.getVar().getName())) {
						JimpleValue val = (JimpleValue)(shadow.getArgsContextValues().get(i));
						result.add(val.getSootValue());
					}
					i++;
				}
				
			// TODO Also check this() and target() in the same manner!
			}
		}
		
		
		
		return result;
	}
	
	/*
	 * Given a definition stmt, update the transferfunction
	 * @param defs
	 * @param ds
	 */
	private void processDefStmt(TransferFunction defs, DefinitionStmt ds, Tree curLocation) {
		if (defs.contains(ds.getLeftOp())) {
			VarTracker vT = defs.get(ds.getLeftOp());
			Tree child = curLocation.addChild(ds);
			vT.addTrace(ds, child);
		}
		
		// Does the LHS of ds contain something already being tracked?
		List<ValueBox> lhsUses = ds.getLeftOp().getUseBoxes();
		for (ValueBox value : lhsUses) {
			if (defs.contains(value.getValue())) {
				// Update the corresponding VarTracker with the definition stmt + trace
				VarTracker vT = defs.get(value.getValue());
				Tree child = curLocation.addChild(ds);
				vT.addTrace(ds, child);
			}
		}
		
		// Is the LHS something global, then we should track it too!
		if (ds.getLeftOp() instanceof StaticFieldRef) {
			VarTracker vT = new VarTracker(ds.getLeftOp());
			Tree child = curLocation.addChild(ds);
			vT.addTrace(ds, child);
			defs.add(vT);
		}

		// Does the RHS of ds contain something we're tracking? If so, we should now track the LHS as well
		List<ValueBox> rhsUses = ds.getRightOp().getUseBoxes();
		for (ValueBox value : rhsUses) {
			if (defs.contains(value.getValue())) {
				VarTracker vT = new VarTracker();
				vT.var=ds.getLeftOp();
				vT.sourceStmt = ds;
				defs.add(vT);
			}
		}
	}
	
	/**************************************************************** 
	 * HELPER METHODS
	 ****************************************************************/
	
	/*
	 * Get the list of join point shadows of a particular advice
	 * (You may want to cast the ShadowMatches that you get to its subclasses
	 * to get something useful out of them..)
	 * @param adv	an advice
	 * @return		list of join point shadows
	 */
	private List<ShadowMatch> getJoinPointShadows(AbstractAdviceDecl adv) {
		GlobalAspectInfo aInfo = Main.v().getAbcExtension().getGlobalAspectInfo();
		ArrayList<ShadowMatch> jps = new ArrayList<ShadowMatch>();

		Set<SootMethod> weavables = WeavableMethods.v().getAll();
		for(SootMethod m: weavables) {
			MethodAdviceList alist = aInfo.getAdviceList(m);
			if (alist==null) {
				continue;
			}
			
			List<AdviceApplication> allAdv = alist.allAdvice();
			for (AdviceApplication adviceApplication : allAdv) {
				if (adviceApplication.advice == adv) {
					jps.add(adviceApplication.shadowmatch);
				}
			}
		}

		return jps;
	}
	
	private boolean isAfterAdvice(AdviceSpec adv) {
		return adv instanceof AfterAdvice 
				|| adv instanceof AfterReturningAdvice
				|| adv instanceof AfterThrowingAdvice;
	}
	
	private boolean isBeforeAdvice(AdviceSpec adv) {
		return adv instanceof BeforeAdvice;
	}
	
	private boolean isAroundAdvice(AdviceSpec adv) {
		return adv instanceof AroundAdvice || adv instanceof BeforeAfterAdvice;
	}

	private void setupAnalysis() {
		// Set up profiling..
		timer = new Timer();
		
		// Set up logging..
		log = Logger.getLogger(this.getClass().getName());
		log.setUseParentHandlers(false);
		
		try {
			ConsoleHandler cHandler = new ConsoleHandler();
			cHandler.setFormatter(new LogFormatter());
			log.addHandler(cHandler);
			
//			FileHandler fHandler = new FileHandler("ajfx-log.txt");
//			fHandler.setFormatter(new LogFormatter());
//			log.addHandler(fHandler);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		// Reset the analysis
//		allPackageCaches = new HashMap<AbstractAdviceDecl, HashSet<String>>();
	}

	/*
	 * Print a message to the debug log
	 * @param message
	 */
	private static void debug(String message) {
		log.log(Level.INFO, message);
	}

	public void enforceSootArgs(List<String> sootArgs) {
		
//		sootArgs.add("-p");
//		sootArgs.add("cg.spark");
		
//		sootArgs.add("-w");
//		sootArgs.add("-app");
//		sootArgs.add("enabled:true");
		sootArgs.add("-keep-line-number");
	}

	public void defaultSootArgs(List<String> sootArgs) {}
	public void setupWeaving() {}
	public void tearDownWeaving() {}
	public void cleanup() {}
}