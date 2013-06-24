package be.ac.ua.ansymo.ajfx;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import soot.Scene;
import soot.jimple.toolkits.callgraph.CallGraph;

import abc.main.AbcTimer;
import abc.main.CompileSequence;
import abc.main.CompilerAbortedException;
import abc.main.CompilerFailedException;
import abc.main.Debug;
import be.ac.ua.ansymo.ajfx.util.TextReader;
import be.ac.ua.ansymo.ajfx.util.Timer;

public class Main extends abc.main.Main {

	public Main(String[] args) throws IllegalArgumentException,	CompilerAbortedException {
		super(args);
	}

	public static String APP_ARGS = "config/test.txt";
//	public static String ANALYSIS = "nw.analysis.soot.extension";
	public static String ANALYSIS = "be.ac.ua.ansymo.ajfx";

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Timer timer = new Timer();
		System.out.println("Application started from: " + new File(".").getAbsoluteFile());
		System.out.println("Reading application arguments from: " + APP_ARGS);
		System.out.println("--------");

		// Set abc cmd-line arguments
		ArrayList<String> abcArgs = new ArrayList<String>();

		// Abc extensions
//		abcArgs.add("-help");
		abcArgs.add("-ext");
		abcArgs.add(ANALYSIS);

		// Add the application-specific arguments from a separate text file
		try {
			ArrayList<String> appArgs = TextReader.read(new File(APP_ARGS));
			abcArgs.addAll(appArgs);
		} catch (IOException exc) {
			System.err.println("Could not read the application arguments file: " + APP_ARGS);
			return;
		}
		
		/* Pseudocode: 
		 * 
		 * Compile initial code + do initial analysis - OK
		 * 
		 * Repeat a bunch of times {
		 * 	Generate a random change + recompile the whole shebang
		 * 	Reanalyse
		 * }
		 */
		
		abc.main.Main.main(abcArgs.toArray(new String[0]));

		// Compiling AST
//		try {
//			String[] args2 = new String[abcArgs.size()];
//			for(int i=0;i<abcArgs.size(); i++) {
//				args2[i]=abcArgs.get(i);
//			}
//			
//			Main m = new Main(args2);
//			m.compileToAst();
//		} catch (Exception e) {
//			e.printStackTrace();
//		}


		System.out.println("Total runtime: " + timer.getTimePassed() + "ms");
	}

	private void compileToAst() throws CompilerFailedException {
		addJarsToClasspath();
		initSoot();

		loadJars();
		loadSourceRoots();

		
		CompileSequence seq = getAbcExtension().getCompileSequence();
		seq = new be.ac.ua.ansymo.ajfx.CompileSequence(getAbcExtension());
		seq.passOptions(aspect_sources, jar_classes, soot_args, polyglot_args, classes_destdir);
//		getAbcExtension().setErrorQueue(createErrorQueue());

		seq.runSequence();
	}

}
