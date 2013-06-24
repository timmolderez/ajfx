package be.ac.ua.ansymo.ajfx;

import java.util.List;

import abc.impact.ImpactAnalysisImpl;
import abc.weaving.weaver.ReweavingPass;

/**
 * Top-level class of the ajfx extension.
 * (abc will look for this class when passing the "-ext be.ac.ua.ansymo.ajfx" command-line parameter to it.)
 * @author Tim Molderez
 */
public class AbcExtension extends abc.main.AbcExtension {
	// Name of our new reweaving pass that will perform the analysis
	private static final ReweavingPass.ID PASS_ID = new ReweavingPass.ID("AJFX");

	protected void createReweavingPasses(List<ReweavingPass> passes) {
		super.createReweavingPasses(passes);

		passes.add(new ReweavingPass(PASS_ID, new Analysis()));
	}

}
