(ns ekeko-ajfx.debug
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [clojure.stacktrace :as st]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use
    [inspector-jay.core]
    [clojure.repl]
    [damp.ekeko logic]
    [damp.ekeko]
    [damp.ekeko visualization]
    [damp.ekeko.visualization.view]
    [clojure.inspector :exclude [inspect]])
  (:import 
    [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph]
    [org.aspectj.lang Signature]
    [java.lang Integer]
    [org.eclipse.jdt.core IJavaElement ITypeHierarchy IType IPackageFragment IClassFile ICompilationUnit
     IJavaProject WorkingCopyOwner IMethod]
    [org.eclipse.jdt.core.dom Expression IVariableBinding ASTParser AST IBinding Type TypeDeclaration 
     QualifiedName SimpleName ITypeBinding MethodDeclaration
     MethodInvocation ClassInstanceCreation SuperConstructorInvocation SuperMethodInvocation
     SuperFieldAccess FieldAccess ConstructorInvocation ASTNode ASTNode$NodeList CompilationUnit]
    [org.aspectj.weaver.patterns Pointcut AndPointcut]))

; Debugging macro, any function can be wrapped in (dbg ) 
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn showUnitCFG
  "Use Ekeko's visualizer to show the unit control-flow graph of a Soot Body"
  [body]
  (let 
    [labelProvider (damp.ekeko.gui.EkekoLabelProvider.)
     graph (new ExceptionalUnitGraph body)
     nodes (-> body .getUnits)
     toString (fn [unit] (str (-> unit (.getTag "LineNumberTag")) ": " (.toString unit)))]
    (ekeko-visualize
      ; nodes
     (into []
            (map vector (map (fn [node] (toString node)) nodes))) 
    
    ; edges
    (into [] (mapcat identity (map (fn [node] ; for each unit
                                     (map (fn [succ] ; for each successor of unit
                                            [(toString node) (toString succ)])
                                          (-> graph (.getSuccsOf node))))
                                   nodes)))
    
    :node|label
    (fn [node] (.getText labelProvider node))
    :node|image 
    (fn [node] (.getImage labelProvider node))
    :edge|style 
    (fn [src dest] edge|directed)
    :layout
    layout|tree)))

(defn showBlockCFG
  "Use Ekeko's visualizer to show the block control-flow graph of a Soot Body"
  [body]
  (let 
    [labelProvider (damp.ekeko.gui.EkekoLabelProvider.)
     graph (new ExceptionalBlockGraph body)
     blocks (-> graph .getBlocks)
     toString (fn [block] (.toString block))]
    (ekeko-visualize
      ; nodes
     (into []
            (map vector (map (fn [block] (toString block)) blocks))) 
    
    ; edges
    (into [] (mapcat identity (map (fn [block] ; for each block
                                     (map (fn [succ] ; for each successor of block
                                            [(toString block) (toString succ)])
                                          (-> graph (.getSuccsOf block))))
                                   blocks)))
    
    :node|label
    (fn [node] (.getText labelProvider node))
    :node|image 
    (fn [node] (.getImage labelProvider node))
    :edge|style 
    (fn [src dest] edge|directed)
    :layout
    layout|tree)))



(showBlockCFG (-> (getAdviceN 0) .getActiveBody))

(defn getAdviceN
  "Get the nth advice body in the system (as a Soot method)" 
  [n]
  (first (nth 
           (ekeko [?method]
                  (l/fresh [?advice]
                           (w/advice ?advice)
                           (ajsoot/advice-soot|method ?advice ?method)))
           n)))

;(inspect (-> (getAdviceN 0) .getActiveBody))

; Construct block graph
(inspect (new BriefBlockGraph (-> (getAdviceN 0) .getActiveBody )))
;
;(inspect (ekeko [?method]
;                  (l/fresh [?advice]
;                           (w/advice ?advice)
;                           (ajsoot/advice-soot|method ?advice ?method))))
;
;(inspect (-> (getAdviceN 7) .getActiveBody))