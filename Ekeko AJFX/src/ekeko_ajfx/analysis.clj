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
    [soot.jimple.internal JimpleLocal JInvokeStmt JIfStmt JGotoStmt JAssignStmt JInstanceFieldRef JNewExpr]
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
(let [diagram (-> (d/new-diagram ["a" "b" "c"])
                (d/add-object #{"bla"})
                (d/add-name #{:1 :2} "d") 
                (d/add-edges "a" "f" :may-mod "b")
                (d/add-edges "d" "g" :may-mod "c")
                (d/add-edges-to-new-object "d" "h" :must-mod "z")
                (d/remove-edges :1 "g" :may-mod))]
  (dbg diagram)
  (dbg (d/find-edges diagram "a" "f" :may-mod))
  (d/reset-obj-id)) 

(defn infer-frame [method]
  (let [body (-> method .getActiveBody)
        units (-> body .getUnits)
        loopTree (new LoopNestTree body)
        diagram (d/new-diagram [])]
    (infer-frame-helper diagram units (-> units .getFirst) (-> units .getLast))))

(defn infer-frame-helper [diagram units unit last-unit]
  (let [unit-type (-> unit .getClass)
        next (cond 
               (instance? IdentityStmt unit) (identity-stmt diagram unit)
               (instance? JAssignStmt unit) (assign-stmt diagram unit)
               (instance? JInvokeStmt unit) nil
               (instance? JGotoStmt unit) (loop-stmt diagram unit units) 
               (instance? JIfStmt unit) (if-stmt diagram unit units) 
               (instance? ReturnStmt unit) (return-stmt diagram unit) 
               :else diagram)
        next-unit (if (or (instance? JGotoStmt unit) (instance? JIfStmt unit))
                    (second next)
                    (-> units (.getSuccOf unit)))
        next-diagram (if (or (instance? JGotoStmt unit) (instance? JIfStmt unit))
                       (first next)
                       next)]
    (if (not (-> unit .equals last-unit))
      (infer-frame-helper next-diagram units next-unit last-unit)
      next-diagram)))

(defn identity-stmt [diagram unit]
  (let [name (-> unit .getLeftOp .getName)]
    (-> (d/add-object diagram [name (str "@" name)])
      (d/add-formal name))))

(defn assign-stmt [diagram unit]
  (let [lhs (-> unit .getLeftOp)
        rhs (-> unit .getRightOp)]
    (cond
      ; Assignment type: a = new c ();
      (instance? rhs JNewExpr) (new-stmt lhs rhs) 
      ; Assignment type: a = b;
      (and (instance? lhs JimpleLocal) (instance? rhs JimpleLocal)) (copy-stmt lhs rhs)
      ; Assignment type: a.f = b;
      (instance? lhs JInstanceFieldRef) true
      ; Assignment type: a = b.f;
      (instance? rhs JInstanceFieldRef) true
      :else (println "Unrecognized type of assignment!"))))

(defn copy-stmt [diagram lhs rhs]
  (let [lhs-name (-> lhs .toString)
        rhs-name (-> rhs .toString)
        rhs-occurrences (d/find-objs-by-name diagram rhs-name)]
    (d/add-name (d/remove-name diagram lhs-name) rhs-occurrences lhs-name)))

(defn field-read-stmt [diagram lhs rhs]
  (let [lhs-name (-> lhs .toString)
        rhs-recv (-> rhs .getBase .toString)
        rhs-field (-> rhs .getField .getName)
        after-rm (d/remove-name diagram lhs-name)
        found-rhs (d/find-objs-by-name diagram rhs-recv) 
        found-read (d/find-edges after-rm rhs-recv rhs-field :may-read)
        found-may (d/find-edges after-rm rhs-recv rhs-field :may-mod)
        found-must (d/find-edges after-rm rhs-recv rhs-field :must-mod)]
    (cond
      (empty? found-read) (d/add-edges-to-new-object diagram rhs-recv rhs-field :may-read lhs-name)
      (empty? found-must) (let [tgts (clojure.set/union 
                                       (for [x found-read] (second x)) 
                                       (for [x found-may] (second x)))]
                            (d/add-name tgts lhs-name))
      :else (d/add-name (for [x found-must] (second x)) lhs-name))))

(defn field-write-stmt [diagram lhs rhs]
  (let [lhs-recv (-> lhs .getBase .toString)
        lhs-field (-> lhs .getField .getName)
        rhs-name (-> rhs .toString)
        lhs-found (d/find-objs-by-name diagram lhs-recv)
        after-rm (if (= 1 (count lhs-found))
                   (-> (d/remove-edges diagram (first lhs-found) lhs-field :may-mod)
                     (d/remove-edges (first lhs-found) lhs-field :must-mod)))]
    (if (or 
          (= 1 (count lhs-found))
          (not (empty? (d/find-edges after-rm lhs-recv lhs-field :must-mod))))
      (d/add-edges after-rm lhs-recv lhs-field :must-mod rhs-name)
      (d/add-edges after-rm lhs-recv lhs-field :may-mod rhs-name))))

(def last-new-id (atom 0))
(defn reset-new-id []
  (swap! last-new-id (fn [x] 0))) 
(defn new-id []
  (keyword (str (swap! last-new-id inc))))

(defn new-stmt [diagram lhs rhs]
  (let [cls-name (-> rhs .getType .toString)
        lhs-name (-> lhs .toString)
        obj-name (str "@" cls-name (new-id))]
    (d/add-object diagram [obj-name] (d/remove-name diagram lhs-name))))

(defn if-stmt [diagram unit units]
  (let [begin-else (-> unit .getTarget)
        end-if (-> units (.getPredOf begin-else))
        end-else (if (instance? JGotoStmt end-if)
                   (-> end-if .getTarget)
                   nil)
        if-diagram (infer-frame-helper diagram units (-> units (.getSuccOf unit)) (-> units (.getPredOf end-if)))
        else-diagram (if (= nil end-else)
                       diagram
                       (infer-frame-helper diagram units begin-else (-> units (.getPredOf end-else))))
        merged-diagram (merge-diagrams if-diagram else-diagram)
        next-unit (if (= nil end-else)
                       begin-else
                       end-else)]
    [merged-diagram next-unit]))

(defn loop-stmt [diagram unit units]
  (let [end-loop (-> unit .getTarget)
        merged-diagram (d/multi-apply diagram
                         (fn [diagram]
                           (infer-frame-helper diagram units 
                             (-> units (.getSuccOf unit)) 
                             (-> units (.getPredOf end-loop))))
                         [[] [] []])]
    [merged-diagram (-> units (.getSuccOf end-loop))]))

(defn compute-mappings [call-diag ctxt-diag formals actuals]
  (let [mapped-roots (d/multi-apply 
                       {}
                       (fn [m root]
                         (let [index (.indexOf formals (subs root 1))]
                           (if (not= index -1)
                             (assoc m (first ((call-diag :names) root)) ((ctxt-diag :names) (nth actuals index)))
                             (assoc m (first ((call-diag :names) root)) #{(d/new-obj-id)}))))
                       (filter
                         (fn [x] (.startsWith x "@"))
                         (keys (call-diag :names))))]
    ))

(defn call-stmt [ctxt-diagram unit]
  (let [method (-> unit .getInvokeExpr .getMethod)
        call-diagram (infer-frame method)
        actuals (-> unit .getInvokeExpr .getArgs)
        call2ctxt (compute-mappings call-diagram ctxt-diagram (call-diagram :formals))]
    )) 

(defn return-stmt [diagram unit]
  (let [value (-> unit .getOp)]
    (if (instance? JimpleLocal)
      (d/add-return-val diagram (-> value .toString))
      diagram)))