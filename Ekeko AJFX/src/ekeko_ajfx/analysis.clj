(ns
  ^{:doc "Defines the frame inference algorithm"
    :author "Tim Molderez" }
  ekeko-ajfx.analysis
  (:use 
    [inspector-jay.core]
    [ekeko-ajfx.diagram])
  (:import 
    [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
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
        loopTest (new LoopNestTree body)]
    loopTest
    ))

(defn copy-stmt [lhs rhs]
  )

(defn field-read-stmt [lhs recv field]
  )

(defn field-assig-stmt [recv field rhs]
  )

(defn new-assig-stmt [lhs rhs-type]
  )

(defn if-stmt [if-body else-body]
  )

(defn loop-stmt [body]
  )

(defn call-stmt [recv static-type meth args]
  )

(defn return-stmt [val]
  )