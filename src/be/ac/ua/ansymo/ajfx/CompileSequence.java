package be.ac.ua.ansymo.ajfx;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import polyglot.util.ErrorInfo;
import polyglot.util.Position;
import polyglot.util.StdErrorQueue;
import abc.ja.jrag.ASTNode;
import abc.ja.jrag.BytecodeParser;
import abc.ja.jrag.CompilationUnit;
import abc.ja.jrag.JavaParser;
import abc.ja.jrag.Problem;
import abc.ja.jrag.Program;
import abc.main.AbcExtension;
import abc.main.CompilerFailedException;
import abc.main.options.OptionsParser;

public class CompileSequence extends abc.main.CompileSequence /*abc.ja.CompileSequence*/ {

	public CompileSequence(AbcExtension ext) {
		super(ext);
	}

	/*public void compile() throws CompilerFailedException, IllegalArgumentException {
		abcExt.setErrorQueue(new StdErrorQueue(System.out, 100, "JastAdd"));
		
		error_queue = abcExt.getErrorQueue();
		if(error_queue == null)
			error_queue = new StdErrorQueue(System.out, 100, "JastAdd");

		try {
			Collection c = new ArrayList();
			c.addAll(aspect_sources);
			if(abc.main.options.OptionsParser.v().verbose())
				c.add("-verbose");
			c.add("-classpath");
			c.add(OptionsParser.v().classpath());
			String[] args = new String[c.size()];
			int index = 0;
			for(Iterator iter = c.iterator(); iter.hasNext(); index++) {
				String s = (String)iter.next();
				args[index] = s;
			}
			Program program = new Program();
			ASTNode.reset();

			program.initBytecodeReader(new BytecodeParser());
			program.initJavaParser(
					new JavaParser() {
						public CompilationUnit parse(InputStream is, String fileName) throws IOException, beaver.Parser.Exception {
							return new abc.ja.parse.JavaParser().parse(is, fileName, error_queue);
						}
					}
					);

			program.initOptions();
			program.addKeyValueOption("-classpath");
			program.addKeyOption("-verbose");
			program.addOptions(args);
			Collection files = program.files();

			//program.addOptions(new String[] { "-verbose" });

			for(Iterator iter = files.iterator(); iter.hasNext(); ) {
				String name = (String)iter.next();
				File file = new File(name);
				if(!file.exists()) {
					error_queue().enqueue(
							ErrorInfo.IO_ERROR,
							"Cannot find source file \"" + name + "\"",
							new Position("NoSuchFile.java")
							);
					throw new CompilerFailedException("There were errors.");
				}
				program.addSourceFile(name);
			}

			for(Iterator iter = jar_classes.iterator(); iter.hasNext(); ) {
				String name = (String)iter.next();
				CompilationUnit u = program.getCompilationUnit(name);
				u.weavableClass = true;
				program.addCompilationUnit(u);
			}

			for(Iterator iter = program.compilationUnitIterator(); iter.hasNext(); ) {
				CompilationUnit unit = (CompilationUnit)iter.next();
				if(unit.fromSource()) {
					// abort if there were syntax or lexical errors
					if(error_queue().errorCount() > 0)
						throw new CompilerFailedException("There were errors.");
				}
			}
			if(Program.verbose())
				System.out.println("Error checking");
			ArrayList errors = new ArrayList();
			ArrayList warnings = new ArrayList();
			program.errorCheck(errors, warnings);
			if(!errors.isEmpty()) {
				Collections.sort(errors);
				for(Iterator iter2 = errors.iterator(); iter2.hasNext(); ) {
					Problem p = (Problem)iter2.next();
					addError(p);
				}
				throw new CompilerFailedException("There were errors.");
			}
			if(!warnings.isEmpty()) {
				Collections.sort(warnings);
				for(Iterator iter2 = warnings.iterator(); iter2.hasNext(); ) {
					Problem p = (Problem)iter2.next();
					addWarning(p);
				}
			}

//			if(Program.verbose())
//				System.out.println("Weaving inter-type declarations");
//			program.generateIntertypeDecls();
//			if(Program.verbose())
//				System.out.println("Flattening Nested Classes");
//			program.transformation();

//			if(Program.verbose())
//				System.out.println("Jimplify1");
//			program.jimplify1();
//			if(Program.verbose())
//				System.out.println("Jimplify2");
//			program.jimplify2();

			abc.main.Main.v().getAbcExtension().getGlobalAspectInfo().buildAspectHierarchy();
			abc.main.AbcTimer.mark("Aspect inheritance");
			abc.main.Debug.phaseDebug("Aspect inheritance");

		} catch (Error polyglot.main.UsageError e) {
			throw (IllegalArgumentException) new IllegalArgumentException("Polyglot usage error: "+e.getMessage()).initCause(e);
		}

		// Output the aspect info
		if (abc.main.Debug.v().aspectInfo)
			abc.main.Main.v().getAbcExtension().getGlobalAspectInfo().print(System.err);
	}

	private void addError(Problem problem) {
		Position p;
		if(problem.column() != -1)
			p = new Position(problem.fileName(), problem.line(), problem.column());
		else
			p = new Position(problem.fileName(), problem.line());
//		error_queue().enqueue(ErrorInfo.SEMANTIC_ERROR, problem.message(), p);
	}

	private void addWarning(Problem problem) {
		Position p;
		if(problem.column() != -1)
			p = new Position(problem.fileName(), problem.line(), problem.column());
		else
			p = new Position(problem.fileName(), problem.line());
//		error_queue().enqueue(ErrorInfo.WARNING, problem.message(), p);
	}*/

}
