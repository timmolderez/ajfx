(ns
  ^{:doc "Defines the frame inference algorithm"
    :author "Tim Molderez" }
  ekeko-ajfx.analysis
  (:use 
    [inspector-jay.core]
    [ekeko-ajfx.diagram])
  (:import 
    [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal JInvokeStmt JIfStmt JGotoStmt JAssignStmt]
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

(defn infer-frame [method]
  (let [body (-> method .getActiveBody)
        units (-> body .getUnits)
        loopTree (new LoopNestTree body)
        diagram (new-diagram [])]
    (infer-frame-helper body (-> units .getFirst) loopTree diagram)))

(defn infer-frame-helper [diagram units unit loopTree]
  (let [unit-type (-> unit .getClass)
        next (cond 
               (instance? IdentityStmt unit) (identity-stmt diagram unit)
               (instance? JAssignStmt unit) "assign"
               (instance? JInvokeStmt unit) "call"
               (instance? JGotoStmt unit) "goto"
               (instance? JIfStmt unit) "if"
               (instance? ReturnStmt unit) "return"
               :else diagram)
        next-diagram next
        next-unit (-> units (.getSuccOf unit))]
    (if (not= next-unit nil)
      (infer-frame-helper next-diagram units next-unit loopTree)
      next-diagram)))

(defn identity-stmt [diagram unit]
  (let [name (-> unit .getLeftOp .getName)]
    (add-object diagram [name (str "@" name)])))

(defn copy-stmt [diagram lhs rhs]
  )

(defn field-read-stmt [diagram lhs recv field]
  )

(defn field-assig-stmt [diagram recv field rhs]
  )

(defn new-assig-stmt [diagram lhs rhs-type]
  )

(defn if-stmt [diagram if-body else-body]
  )

(defn loop-stmt [diagram body]
  )

(defn call-stmt [diagram recv static-type meth args]
  )

(defn return-stmt [diagram val]
  )