![ajFX logo](https://raw.githubusercontent.com/timmolderez/ajfx/master/ajFX.png)


ajFX provides a frame inference analysis for Java and AspectJ applications. It is a static, compositional source code analysis that is able to infer which locations may/must be modified by a given method or advice body (and whatever code is reachable from that body). These locations are described such that they can be understood in terms of the body's pre-state, i.e. in terms of (fields of) formal parameters or the this object.


## Requirements

- [Eclipse](https://www.eclipse.org/) (tested using Eclipse Kepler and Luna)
- [Ekeko](https://github.com/cderoove/damp.ekeko), a meta-programming library to reason about Java code
- For AspectJ support: AJDT (tested on version 2.2.3) and [Ekeko's AspectJ extension](https://github.com/cderoove/damp.ekeko.aspectj)
