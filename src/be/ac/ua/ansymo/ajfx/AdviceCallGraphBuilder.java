package be.ac.ua.ansymo.ajfx;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import abc.da.weaving.weaver.depadviceopt.ds.WeavableMethods;
import abc.main.Main;
import abc.weaving.aspectinfo.AbstractAdviceDecl;
import abc.weaving.aspectinfo.AdviceDecl;
import abc.weaving.aspectinfo.GlobalAspectInfo;
import abc.weaving.aspectinfo.MethodSig;

import soot.EntryPoints;
import soot.G;
import soot.Local;
import soot.MethodOrMethodContext;
import soot.PointsToAnalysis;
import soot.PointsToSet;
import soot.Scene;
import soot.SootMethod;
import soot.Type;
import soot.jimple.toolkits.callgraph.CallGraph;
import soot.jimple.toolkits.callgraph.ContextInsensitiveContextManager;
import soot.jimple.toolkits.callgraph.ContextManager;
import soot.jimple.toolkits.callgraph.OnFlyCallGraphBuilder;
import soot.jimple.toolkits.callgraph.ReachableMethods;
import soot.util.queue.QueueReader;

/**
 * Advice call graph builder - This class will construct a call graph, using only all advice as entry points!
 * @author Tim Molderez
 *  
 * Note - This is an exact copy of soot.jimple.toolkits.callgraph.CallGraphBuilder, with some minor adjustments in the constructor.
 * (Couldn't extend as it was a final class..)
 */
public class AdviceCallGraphBuilder { 
	private PointsToAnalysis pa;
	private final ReachableMethods reachables;
	private final OnFlyCallGraphBuilder ofcgb;
	private final CallGraph cg;

	public CallGraph getCallGraph() { return cg; }
	public ReachableMethods reachables() { return reachables; }

	public static ContextManager makeContextManager( CallGraph cg ) {
		return new ContextInsensitiveContextManager( cg );
	}

	/** This constructor builds a complete call graph using the given
	 * PointsToAnalysis to resolve virtual calls. */
	public AdviceCallGraphBuilder( PointsToAnalysis pa ) {
		this.pa = pa;
		cg = new CallGraph();
		Scene.v().setCallGraph( cg );
		reachables = Scene.v().getReachableMethods();
		ContextManager cm = makeContextManager(cg);
		ofcgb = new OnFlyCallGraphBuilder( cm, reachables );
	}

	/** This constructor builds the incomplete hack call graph for the
	 * Dava ThrowFinder.
	 * It uses all application class methods as entry points, and it ignores
	 * any calls by non-application class methods.
	 * Don't use this constructor if you need a real call graph. */
	public AdviceCallGraphBuilder() {
		G.v().out.println( "Warning: using incomplete callgraph containing "+
				"only application classes." );
		pa = soot.jimple.toolkits.pointer.DumbPointerAnalysis.v();
		cg = new CallGraph();
		Scene.v().setCallGraph(cg);
		List<MethodOrMethodContext> entryPoints = new ArrayList<MethodOrMethodContext>();

		// Use all advice as the entry points
		GlobalAspectInfo globalAspectInfo = Main.v().getAbcExtension().getGlobalAspectInfo();
		
		for (Iterator<AbstractAdviceDecl> adDeclIt = globalAspectInfo.getAdviceDecls().iterator(); adDeclIt.hasNext(); ) {
			AbstractAdviceDecl aadDecl = adDeclIt.next();
			if ((aadDecl instanceof AdviceDecl)) {
				
				SootMethod m = ((AdviceDecl) aadDecl).getImpl().getSootMethod();
				entryPoints.add(m);
			}
		}
		
		// Add all joinpoint shadows as entry points as well
		Set<SootMethod> weavables = WeavableMethods.v().getAll();
		for(SootMethod m: weavables) {
			entryPoints.add(m);
		}
//		globalAspectInfo.getAdviceList();

		reachables = new ReachableMethods( cg, entryPoints );
		ContextManager cm = new ContextInsensitiveContextManager( cg );
		ofcgb = new OnFlyCallGraphBuilder( cm, reachables, true );
	}

	public void build() {
		QueueReader worklist = reachables.listener();
		while(true) {
			ofcgb.processReachables();
			reachables.update();
			if( !worklist.hasNext() ) break;
			MethodOrMethodContext momc = (MethodOrMethodContext) worklist.next();
			List receivers = (List) ofcgb.methodToReceivers().get(momc.method());
			if( receivers != null) for( Iterator receiverIt = receivers.iterator(); receiverIt.hasNext(); ) {     
				final Local receiver = (Local) receiverIt.next();
				final PointsToSet p2set = pa.reachingObjects( receiver );
				for( Iterator typeIt = p2set.possibleTypes().iterator(); typeIt.hasNext(); ) {
					final Type type = (Type) typeIt.next();
					ofcgb.addType( receiver, momc.context(), type, null );
				}
			}
			List stringConstants = (List) ofcgb.methodToStringConstants().get(momc.method());
			if( stringConstants != null ) for( Iterator stringConstantIt = stringConstants.iterator(); stringConstantIt.hasNext(); ) {     
				final Local stringConstant = (Local) stringConstantIt.next();
				PointsToSet p2set = pa.reachingObjects( stringConstant );
				Collection possibleStringConstants = p2set.possibleStringConstants();
				if( possibleStringConstants == null ) {
					ofcgb.addStringConstant( stringConstant, momc.context(), null );
				} else {
					for( Iterator constantIt = possibleStringConstants.iterator(); constantIt.hasNext(); ) {
						final String constant = (String) constantIt.next();
						ofcgb.addStringConstant( stringConstant, momc.context(), constant );
					}
				}
			}
		}
	}
}

