(ns
  ^{:doc "Defines the frame inference algorithm"
    :author "Tim Molderez" }
  ekeko-ajfx.analysis
  (:use 
    [inspector-jay.core])
  (:require 
    [ekeko-ajfx.diagram :as d])
  (:import 
    [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal JInvokeStmt JIfStmt JGotoStmt JAssignStmt JInstanceFieldRef]
    [soot.jimple ThisRef ParameterRef ReturnStmt]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph LoopNestTree]
    [org.aspectj.lang Signature]
    [java.lang Integer]
    [org.eclipse.jdt.core IJavaElement ITypeHierarchy IType IPackageFragment IClassFile ICompilationUnit
     IJavaProject WorkingCopyOwner IMethod]
    [org.eclipse.jdt.core.dom Expression IVariableBinding ASTParser AST IBinding Type TypeDeclaration 
     QualifiedName SimpleName ITypeBinding MethodDeclaration
     MethodInvocation ClassInstanceCreation SuperConstructorInvocation SuperMethodInvocation
     SuperFieldAccess FieldAccess ConstructorInvocation ASTNode ASTNode$NodeList CompilationUnit]
    [org.aspectj.weaver.patterns Pointcut AndPointcut]))


; Test case
(-> (new-diagram ["a" "b" "c"]) 
  (add-object #{"bla"})
  (add-edges "a" "f" :may-mod "b"))


(defn infer-frame [method]
  (let [body (-> method .getActiveBody)
        units (-> body .getUnits)
        loopTree (new LoopNestTree body)
        diagram (d/new-diagram [])]
    (infer-frame-helper diagram units (-> units .getFirst) loopTree)))

(defn infer-frame-helper [diagram units unit loopTree]
  (let [unit-type (-> unit .getClass)
        next (cond 
               (instance? IdentityStmt unit) (identity-stmt diagram unit)
               (instance? JAssignStmt unit) (assign-stmt diagram unit)
               (instance? JInvokeStmt unit) nil
               (instance? JGotoStmt unit) nil
               (instance? JIfStmt unit) nil
               (instance? ReturnStmt unit) (return-stmt diagram unit) 
               :else diagram)
        next-unit (if (or (instance? JGotoStmt unit) (instance? JIfStmt unit))
                    (second next)
                    (-> units (.getSuccOf unit)))
        next-diagram (if (or (instance? JGotoStmt unit) (instance? JIfStmt unit))
                       (first next)
                       next)]
    (if (not= next-unit nil)
      (infer-frame-helper next-diagram units next-unit loopTree)
      next-diagram)))

(defn identity-stmt [diagram unit]
  (let [name (-> unit .getLeftOp .getName)]
    (d/add-object diagram [name (str "@" name)])))



(defn assign-stmt [diagram unit]
  (let [lhs (-> unit .getLeftOp)
        rhs (-> unit .getRightOp)]
    (cond
      ; Assignment type: a = new c ();
      (instance? rhs JNewExpr) (new-stmt lhs rhs) 
      ; Assignment type: a = b;
      (and (instance? lhs JimpleLocal) (?instance rhs JimpleLocal)) (copy-stmt lhs rhs)
      ; Assignment type: a.f = b;
      (instance? lhs JInstanceFieldRef) true
      ; Assignment type: a = b.f;
      (instance? rhs JInstanceFieldRef) true
      :else (println "Unrecognized type of assignment!"))))

(defn copy-stmt [diagram lhs rhs]
  (let [lhs-name (-> lhs .toString)
        rhs-name (-> rhs .toString)
        rhs-occurrences (d/find-objs-by-name diagram rhs-name)]
    (d/add-name (d/remove-name diagram lhs-name) rhs-occurrences lhs-name)
    
    ))

(defn field-read-stmt [diagram lhs rhs]
  (let [lhs-name (-> lhs .toString)
        rhs-recv (-> rhs .getBase .toString)
        rhs-field (-> rhs .getField .getName)
        after-rm (d/remove-name diagram lhs-name)
        found-read (find-edges after-rm rhs-recv rhs-field :may-read)
        found-may (find-edges after-rm rhs-recv rhs-field :may-mod)
        found-must (find-edges after-rm rhs-recv rhs-field :must-mod)]
    (cond
      ())
    ))

(defn field-write-stmt [diagram lhs rhs]
  )

(def last-new-id (atom 0))
(defn reset-new-id []
  (swap! last-obj-id (fn [x] 0))) 
(defn new-id []
  (keyword (str (swap! last-obj-id inc))))

(defn new-stmt [diagram lhs rhs]
  (let [cls-name (-> rhs .getType .toString)
        lhs-name (-> lhs .toString)
        obj-name (str "@" cls-name (new-id))]
    (d/add-object diagram [] (d/remove-name diagram lhs-name))))

(defn if-stmt [diagram if-body else-body]
  )

(defn loop-stmt [diagram body]
  )

(defn call-stmt [diagram recv static-type meth args]
  )

(defn return-stmt [diagram unit]
  (let [value (-> unit .getOp)]
    (if (instance? JimpleLocal)
      (add-return-val diagram (-> value .toString))
      diagram)))