package be.ac.ua.ansymo.ajfx.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import abc.da.weaving.weaver.depadviceopt.ds.WeavableMethods;
import abc.main.Main;
import abc.weaving.aspectinfo.AbstractAdviceDecl;
import abc.weaving.aspectinfo.GlobalAspectInfo;
import abc.weaving.matching.AdviceApplication;
import abc.weaving.matching.MethodAdviceList;
import abc.weaving.matching.ShadowMatch;
import be.ac.ua.ansymo.ajfx.containers.Tree;

import soot.MethodOrMethodContext;
import soot.Scene;
import soot.SootClass;
import soot.SootMethod;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.Edge;
import soot.jimple.toolkits.callgraph.ReachableMethods;
import soot.util.dot.DotGraph;
import soot.util.dot.DotGraphEdge;
import soot.util.dot.DotGraphNode;
import soot.util.queue.QueueReader;

/**
 * This class provides all sorts of helper methods that are useful to the incremental reanalysis
 * @author Tim Molderez
 *
 */
public class SootUtils {

	private CallGraph cg;

	/**
	 * Constructor
	 * @param cg
	 */
	public SootUtils(CallGraph cg) {
		this.cg = cg;
	}

	/**
	 * Does this method override another method?
	 * @param m
	 * @return
	 */
	public boolean isOverriding(SootMethod m) {
		String sig = m.getSubSignature();
		SootClass cls = m.getDeclaringClass();
		boolean isOverriding = false;
		while (cls.hasSuperclass()) {
			cls = cls.getSuperclass();
			cls.getMethod(sig);
			cls.declaresMethod(sig);
		}
		return isOverriding;
	}

	/*
	/**
	 * Is method a reachable from method b? In other words, could b ever call a?
	 * @param cg
	 * @param a
	 * @param b
	 * @return
	 */
	/*public boolean isReachableFrom(SootMethod a, SootMethod b) {

		// You don't use reachableMethods like this!! It's passed and constructed when building the call graph..

		ArrayList<MethodOrMethodContext> rmList = new ArrayList<MethodOrMethodContext>();
		rmList.add(b);
		ReachableMethods rm = new ReachableMethods(cg, rmList);
		return rm.contains(a);
	}*/

	/**
	 * Finds all paths in the call graph starting at method a, ending in method b.
	 * In other words, finds all ways in which b is reachable from a.
	 * @param a		start searching here
	 * @param b		look for calls to this method body
	 * @return		a tree with root a, and b in all leafs. Each path from the root ending in a leaf is a path from a to b.
	 */
	public Tree<Object> findPaths(SootMethod a, SootMethod b) {
		return depthFirstSearch(a, b, new HashSet<Edge>());
	}

	/**
	 * Prints each edge of the call graph to console
	 */
	public void printCallGraph() {
		Iterator<Edge> it = cg.listener();
		while( it.hasNext() ) {
			soot.jimple.toolkits.callgraph.Edge e =
					(soot.jimple.toolkits.callgraph.Edge) it.next();
			System.out.println("Src:" + e.src());
			System.out.println("Stm:" + e.srcStmt());
			System.out.println("Knd:" + e.kind());
			System.out.println("Tgt:" + e.tgt());
			System.out.println("");
			//            System.out.println(""+e.src()+e.srcStmt()+" ==== "+e.kind()+" ===> "+e.tgt());
		}
	}

	/**
	 * Draw the call graph
	 * @param name
	 */
	public DotGraph exportCallGraphToDot(String name) {
		DotGraph dot = new DotGraph(name);
		dot.setGraphLabel(name);

		int id = 0;
		Map<SootMethod, Integer> idmap = new HashMap<SootMethod, Integer>();

		// Draw nodes
		QueueReader<Edge> reader = cg.listener(); // Iterator over all edges
		while (reader.hasNext()) {
			Edge edge = reader.next();

			SootMethod src = (SootMethod)edge.getSrc();
			SootMethod tgt = (SootMethod)edge.getTgt();
			
			// Draw src (if not drawn yet)
			if (!idmap.containsKey(src)) {
				DotGraphNode label = dot.drawNode("head"+id);
				idmap.put(src, new Integer(id));
				label.setLabel(src.toString());
				label.setAttribute("fontsize","18");
				label.setShape("box");
				id++;
			}
			
			// Draw tgt
			if (!idmap.containsKey(tgt)) {
				DotGraphNode label = dot.drawNode("head"+id);
				idmap.put(tgt, new Integer(id));
				label.setLabel(tgt.toString());
				label.setAttribute("fontsize","18");
				label.setShape("box");
				id++;
			}
			
			// Draw edge between src and tgt
			DotGraphEdge dotEdge = dot.drawEdge("head"+idmap.get(src), "head"+idmap.get(tgt));

		}
		
		return dot;
	}

		private Tree<Object> depthFirstSearch(SootMethod a, SootMethod b, Set<Edge> visited) {
			Tree<Object> paths = new Tree(a);
			Iterator<Edge> it = cg.edgesOutOf(a);

			while(it.hasNext()) {
				Edge e = it.next();
				if (!visited.contains(e)) {
					visited.add(e);
					MethodOrMethodContext mmc = e.getTgt();
					if (mmc instanceof SootMethod) {
						Tree<Object> subPaths = depthFirstSearch((SootMethod)mmc,b, visited);
						if (subPaths!=null) {
							paths.addChild(subPaths);
						} 
					}
				}
			}

			if (paths.getNumberOfChildren() != 0 || a.getSignature().equals(b.getSignature())) {
				return paths;
			} else {
				return null;
			}
		}

		/**
		 * Retrieve all joinpoint shadows of a particular advice
		 * @param adv
		 */
		public List<ShadowMatch> getJoinPointShadows(AbstractAdviceDecl adv) {
			GlobalAspectInfo aInfo = Main.v().getAbcExtension().getGlobalAspectInfo();
			ArrayList<ShadowMatch> jps = new ArrayList<ShadowMatch>();

			Set<SootMethod> weavables = WeavableMethods.v().getAll();
			for(SootMethod m: weavables) {
				MethodAdviceList alist = aInfo.getAdviceList(m);
				List<AdviceApplication> allAdv = alist.allAdvice();
				for (AdviceApplication adviceApplication : allAdv) {
					if (adviceApplication.advice == adv) {
						jps.add(adviceApplication.shadowmatch);
					}
				}
			}

			return jps;
		}
	}
