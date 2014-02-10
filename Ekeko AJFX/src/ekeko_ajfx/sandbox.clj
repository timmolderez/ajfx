(ns
  ^{:doc "Extra relations to query Soot control-flow graph."
    :author "Tim Molderez" }
  ekeko-ajfx.sandbox
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use ;[inspector-jay.core]
        [clojure.repl]
        [damp.ekeko logic]
        [damp.ekeko]
        [damp.ekeko visualization]
        [damp.ekeko.visualization.view]
        [clojure.inspector :exclude [inspect]])
  (:import [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
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

(defn showCflowGraph
  "Use Ekeko's visualizer to show the control-flow graph of a soot.Body"
  [body]
  (let 
    [labelProvider (damp.ekeko.gui.EkekoLabelProvider.)
     graph (new ExceptionalUnitGraph body)
     nodes (-> body .getUnits)
     
     edges nil]
    (ekeko-visualize
      ; nodes
      [[1] [(new String "bla")] [3]]
      ; edges
      [[1 "bla"] ["bla" 3] [3 "bla"]]
      :node|label
      (fn [typedeclaration] (.getText labelprovider typedeclaration))
      :node|image 
      (fn [typedeclaration] (.getImage labelprovider typedeclaration))
      :edge|style 
      (fn [src dest] edge|directed)
      :edge|label 
      (fn [edge] "test")
      :layout
      layout|horizontaltree)))

 (def gMeth (second (first (ekeko [?a ?b] (ajsoot/advice-soot|method ?a ?b)))))
 
 (def gGraph (new ExceptionalUnitGraph (-> gMeth .getActiveBody)))

(showCflowGraph (-> gMeth .getActiveBody)) 
 

(-> (new Object) .toString)
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
      layout|horizontaltree
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
  


    (comment (defn
  demo-visualization
  []
  (let [labelprovider (damp.ekeko.gui.EkekoLabelProvider.)]
    (ekeko-visualize
      ;nodes
                   (ekeko [?t] 
             (fresh [?root]
                    (typedeclaration-name|qualified|string ?root "be.ac.chaq.model.ast.java.Expression")
                    (conde [(equals ?t ?root)]
                           [(typedeclaration-typedeclaration|super ?t ?root)])))
      ;edges
                   (ekeko [?fromuser ?totype]
             (fresh [?anno ?annotypelit ?annotype]
                    (annotation|ep-typeliteral ?anno ?annotypelit)
                    (ast-typedeclaration|encompassing ?anno ?fromuser)
                    (typeliteral-type ?annotypelit ?annotype)
                    (typedeclaration-type ?totype ?annotype)))
      :node|label
      (fn [typedeclaration] 
        (.getText labelprovider  typedeclaration))
      :node|image 
      (fn [typedeclaration] 
        (.getImage labelprovider typedeclaration))
      :edge|style 
      (fn [src dest] edge|directed)
      :layout
      layout|horizontaltree))))