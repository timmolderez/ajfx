package be.ac.ua.ansymo.ajfx;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import be.ac.ua.ansymo.ajfx.containers.Tree;
import be.ac.ua.ansymo.ajfx.containers.Tree.Filter;
import be.ac.ua.ansymo.ajfx.util.SootUtils;

import abc.main.Main;
import abc.weaving.aspectinfo.AbstractAdviceDecl;
import abc.weaving.aspectinfo.AdviceDecl;
import abc.weaving.aspectinfo.Aspect;
import abc.weaving.aspectinfo.GlobalAspectInfo;
import abc.weaving.matching.ShadowMatch;
import soot.Hierarchy;
import soot.MethodOrMethodContext;
import soot.Scene;
import soot.SootMethod;
import soot.jimple.Stmt;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.ReachableMethods;

/**
 * This class makes the def-use analysis incremental
 * For each type of change that can occur, the class provides a method that will 
 * invalidate parts of the analysis's results, and re-analyse those parts
 * @author Tim Molderez
 *
 */
public class IncrementalAnalysis {
	
	private CallGraph cGraph;
	private HashMap<AbstractAdviceDecl, HashSet<String>> pCache;
//	private HashMap<AdviceDecl,AdviceEffect> fxMap;
	
	private GlobalAspectInfo aInfo;
	private SootUtils util;

	/** Constructor
	 * @param cg
	 * @param packageCache
	 * @param effects
	 */
	public IncrementalAnalysis(CallGraph cg, HashMap<AbstractAdviceDecl, HashSet<String>> packageCache) {
		cGraph = cg;
		pCache = packageCache;
		aInfo = Main.v().getAbcExtension().getGlobalAspectInfo();
		util = new SootUtils(cg);
	}
	
	// METHODS //////////////////////////////////////////////////////////////

	public void methodAdded(SootMethod newMeth) {
		Hierarchy types = Scene.v().getActiveHierarchy();
		
		
		if (util.isOverriding(newMeth)) {
			// Adjust write traces
			for(AbstractAdviceDecl a:aInfo.getAdviceDecls()) {
				
				SootMethod m = ((AdviceDecl)a).getImpl().getSootMethod();
				
				Tree<Object> paths = util.findPaths(m, newMeth);
				if (paths!=null) {
					/* TODO - For all leafs, do the analysis .. meaning:
					 * Find all writes, starting from each leaf. Then find new reads, starting from each joinpoint shadow
					 */
					
				}
				
//				// Remove all traces that no longer apply (because the overridden method no longer gets called)
//				Tree<Object> writes = fxMap.get(a).writeTraces;
//				ArrayList<Tree<Object>> found = writes.findNodes(newMeth, new Tree.Filter<Object>() {
//					public boolean matches(Object searchKey, Object current) {
//						SootMethod key = (SootMethod)searchKey;
//						SootMethod cur = (SootMethod)current;
//						// TODO Perhaps not store SootMethods in there, but statements?
//						return false;
//					}
//					
//				});
//				for (Tree<Object> tree : found) {
//					// Remove write traces that were found
//					writes.removeDanglingPath(tree);
//					// Remove the corresponding read traces
//					fxMap.get(a).readTraces.remove(tree.getContents());
//				}
				
				
				
			}
			
			// Adjust read traces: Find all read 
			for(AbstractAdviceDecl a:aInfo.getAdviceDecls()) {
				String pkg = newMeth.getDeclaringClass().getPackageName();
				
				if (pCache.get(a).contains(pkg)) {
					// Do any of the shadows ever call the new method?
					
					for(ShadowMatch shadow:util.getJoinPointShadows(a)) {
//						Tree<Object> paths = util.findPaths(m, newMeth);
						
//						for(Stmt assignment: fxMap.get(a).readTraces.keySet()) {
//							Tree<Object> reads = fxMap.get(a).readTraces.get(assignment);
//							ArrayList<Tree<Object>> found = reads.findNodes(newMeth, new Tree.Filter<Object>() {
//								public boolean matches(Object searchKey, Object current) {
//									SootMethod key = (SootMethod)searchKey;
//									SootMethod cur = (SootMethod)current;
//									// TODO Perhaps not store SootMethods in there, but statements?
//									return false;
//								}
//								
//							});
//							
//							
//						}
					}
				}
			}
			
		}
		
		// If not overriding, nothing happens, because the added method is never called..
		
	}

	public void methodRemoved() {

	}
	
	public void methodModified_addCall() {

	}
	
	public void methodModified_removeCall() {

	}
	
	public void methodModified_addAssignment() {

	}
	
	public void methodModified_removeAssignment() {

	}
	
	// FIELDS //////////////////////////////////////////////////////////////
	
	public void addField() {
		
	}
	
	public void removeField() {
		
	}
	
	// ADVICE //////////////////////////////////////////////////////////////
	
	public void adviceAdded() {

	}

	public void adviceRemoved() {

	}
	
	public void adviceModified_addCall() {

	}
	
	public void adviceModified_removeCall() {

	}
	
	public void adviceModified_addAssignment() {

	}
	
	public void adviceModified_removeAssignment() {

	}
	
	// MISC ASPECT CHANGES //////////////////////////////////////////////////////////////
	public void pointcutModified() {

	}
	
	public void precedenceModified() {

	}
	
	public void parentsModified() {

	}
}
