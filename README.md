![ajFX logo](https://raw.githubusercontent.com/timmolderez/ajfx/master/ajFX.png)

ajFX provides a frame inference analysis for Java and AspectJ applications. It is a static, compositional source code analysis that is able to infer which locations may/must be modified by a given method or advice body (and whatever code is reachable from that body). These locations are described such that they can be understood in terms of the body's pre-state, i.e. in terms of (fields of) formal parameters or the this object.


## Getting started

To get started, you will need the following:

- [Eclipse](https://www.eclipse.org/) (tested using Eclipse Kepler and Luna)
- [Ekeko](https://github.com/cderoove/damp.ekeko), a meta-programming library to reason about Java code
- For AspectJ support (optional): [AJDT](http://www.eclipse.org/ajdt/) (tested on version 2.2.3) and [Ekeko's AspectJ extension](https://github.com/cderoove/damp.ekeko.aspectj)

Once this is done:

- Clone/download this project
- Import the project in Eclipse
- Run it as an 'Eclipse application' to start a new instance of Eclipse
- In this new instance, open/import whatever Java/AspectJ project you want to analyse
- Right-click the project > Configure > Include in Ekeko queries
- Right-click the project again > Configure > Ekeko Properties > Fill in ```-no-bodies-for-excluded -src-prec c -f jimple -keep-line-number -app -w -p jb use-original-names:true -p cg.cha``` in the "Soot arguments:" text box.
- Right-click the project again > Configure > Enable Ekeko Soot Analyses (Depending on the size of the project, this might take a few seconds up to a few minutes; the [Soot](http://www.sable.mcgill.ca/soot/) framework is currently converting the project to Jimple code, which is the intermediate representation that ajFX uses..)
- Ekeko > start nREPL (This should open up a Clojure REPL in the original Eclipse instance.)
- In the original Eclipse instance, open up the ekeko_ajfx/core.clj file
- Clojure > Load file in REPL (This should take less than a minute..)
- Clojure > Switch REPL to File's Namespace
- ajFX is now at your disposal (and Ekeko and Soot are too). Try running ```(inspect (analyse-all-bodies))``` to analyse all method/advice bodies, and open an [Inspector](https://github.com/timmolderez/inspector-jay) window to browse the results.
