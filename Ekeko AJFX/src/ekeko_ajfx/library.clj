(ns
  ^{:doc "A library of aliasing diagrams for various method bodies (in case we don't have access to the body)"
    :author "Tim Molderez" }
  ekeko-ajfx.library
  (:require 
    [ekeko-ajfx.diagram :as d])
  (:import
    [java.util HashSet]
    [soot SootMethod Unit PatchingChain PrimType VoidType] 
    [soot.jimple IdentityStmt Stmt]
    [soot.jimple.internal JimpleLocal JInvokeStmt JStaticInvokeExpr JIfStmt JGotoStmt JAssignStmt JInstanceFieldRef JNewExpr JTableSwitchStmt JIdentityStmt]
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

(def frame-library {:Test
                    {:helper3 (d/new-diagram [])}
                    :helper4 (-> (d/new-diagram [])
                               (d/add-object "@this")
                               (d/add-formal "this")
                               (d/add-edges-to-new-object "@this" :must-mod "tgt"))
                    })



(defn generate-default-frame [method]
  (let [return-type (-> method .getReturnType)
        returns-void (instance? VoidType return-type)
        returns-primitive (instance? PrimType return-type)]
    (cond 
      returns-void (d/new-diagram [])
      :else (-> (d/new-diagram [])
              (d/add-object [d/ANY-OBJ])
              (d/add-return-val [d/ANY-OBJ])))))

(defn get-frame-from-library [method]
  (let [entry (frame-library (keyword (-> method .getDeclaringClass .getJavaStyleName)))]
    (if (= nil entry)
      (generate-default-frame method)
      (let [m-entry (entry (keyword (-> method .getName)))]
        (if (= nil m-entry)
          (generate-default-frame method)
          m-entry)))))