(ns
  ^{:doc "Just a sandbox to tinker and e"
    :author "Tim Molderez" }
  ekeko-ajfx.sandbox
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [clojure.stacktrace :as st]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use [inspector-jay.core]
        [clojure.repl]
        [damp.ekeko logic]
        [damp.ekeko]
        [damp.ekeko visualization]
        [damp.ekeko.visualization.view]
        [clojure.inspector :exclude [inspect]])
  (:import [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [soot.toolkits.graph ExceptionalUnitGraph]
    [org.aspectj.lang Signature]
    [java.lang Integer]
    [org.eclipse.jdt.core IJavaElement ITypeHierarchy IType IPackageFragment IClassFile ICompilationUnit
     IJavaProject WorkingCopyOwner IMethod]
    [org.eclipse.jdt.core.dom Expression IVariableBinding ASTParser AST IBinding Type TypeDeclaration 
     QualifiedName SimpleName ITypeBinding MethodDeclaration
     MethodInvocation ClassInstanceCreation SuperConstructorInvocation SuperMethodInvocation
     SuperFieldAccess FieldAccess ConstructorInvocation ASTNode ASTNode$NodeList CompilationUnit]
    [org.aspectj.weaver.patterns Pointcut AndPointcut]
    ))




(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn showCflowGraph2
  "Use Ekeko's visualizer to show the control-flow graph of a soot.Body"
  [body]
  (let 
    [labelProvider (damp.ekeko.gui.EkekoLabelProvider.)
     graph (new ExceptionalUnitGraph body)
     nodes (-> body .getUnits)]
    (ekeko-visualize
      ; nodes
      (into [] 
            (map vector (map (fn [node] (.toString node)) nodes))) 
    
    ; edges
    (into [] (mapcat identity (map (fn [node] ; for each unit
                                     (map (fn [succ] ; for each successor of unit
                                            [(.toString node) (.toString succ)])
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

(ekeko-visualize nil nil)

; edge format [[1 "bla"] ["bla" 3] [3 "bla"]]

 (def gMeth (second (first (ekeko [?a ?b] (ajsoot/advice-soot|method ?a ?b)))))
 (showCflowGraph2 (-> gMeth .getActiveBody))
 
 (.toString (first (-> gMeth .getActiveBody .getUnits)))
 
(def gGraph (new ExceptionalUnitGraph (-> gMeth .getActiveBody)))
(-> gGraph (.getSuccsOf (first (-> gMeth .getActiveBody .getUnits))) )



(
 
(-> (first (nth (ekeko [?a] (w/advice ?a)) 1)) .getDeclaringAspect)

(let [labelprovider (damp.ekeko.gui.EkekoLabelProvider.)]
      (ekeko-visualize
        ; nodes
       [[1] [(new String "bla")] [3]]
        ; edges
       [[1 "bla"] ["bla" 3] [3 "bla"]]
    
        :node|label
      (fn [typedeclaration] 
        (.getText labelprovider typedeclaration))
    :node|image 
      (fn [typedeclaration] 
        (.getImage labelprovider typedeclaration))
    :edge|style 
      (fn [src dest] edge|directed)
    :edge|label 
      (fn [edge] "test")
    :layout
      layout|tree
        ))




    (let [labelprovider (damp.ekeko.gui.EkekoLabelProvider.)]
      (ekeko-visualize
        ; nodes
       (ekeko [?str] 
               (l/fresh [?a] 
                 (w/advice ?a)
                 (equals ?str (-> ?a .getDeclaringAspect))))
        ; edges
       nil
    
        :node|label
      (fn [typedeclaration] 
        (.getText labelprovider  typedeclaration))
    :node|image 
      (fn [typedeclaration] 
        (.getImage labelprovider typedeclaration))
    :edge|style 
      (fn [src dest] edge|directed)
    :layout
      layout|horizontaltree
        ))
  