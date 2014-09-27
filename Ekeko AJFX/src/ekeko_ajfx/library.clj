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

; Hash map of aliasing diagrams for a number of SootMethods
; The keys in this map are the result of (hash (-> method .getSignature))
(def frame-library {
                  ; <java.util.Hashtable: java.lang.Object remove(java.lang.Object)>>
                  :1307823846 (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "elements" :must-mod d/ANY-OBJ))
                    
                  ; <java.util.Hashtable: java.lang.Object put(java.lang.Object,java.lang.Object)>>
                  :-2052540966 (-> (d/new-diagram [])
                                   (d/add-object ["@this"])
                                   (d/add-formal "this")
                                   (d/add-object [d/ANY-OBJ])
                                   (d/add-edges "@this" "elements" :must-mod d/ANY-OBJ))
                    
                  ;<java.util.Vector: void addElement(java.lang.Object)>
                  :82825515 (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "elements" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.Frame: java.awt.Component add(java.awt.Component)>
                  :-908026355
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "components" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.MenuItem: void setActionCommand(java.lang.String)>
                  :-2118539535
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "actionCommands" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.Menu: java.awt.MenuItem add(java.awt.MenuItem)>
                  :1196070556
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "items" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.Menu: void addActionListener(java.awt.event.ActionListener)>
                  :-656114926
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "actionListeners" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.MenuBar: java.awt.Menu add(java.awt.Menu)>
                  :-1881769299
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "menus" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.Frame: void setSize(java.awt.Dimension)>>
                  :-1742250182
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "width" :must-mod d/ANY-OBJ)
                                (d/add-edges "@this" "height" :must-mod d/ANY-OBJ))
                    
                  ;<java.awt.Frame: void setVisible(boolean)>
                  :-1838780473
                  (-> (d/new-diagram [])
                                  (d/add-object ["@this"])
                                  (d/add-formal "this")
                                  (d/add-object [d/ANY-OBJ])
                                  (d/add-edges "@this" "visible" :must-mod d/ANY-OBJ))
                  })

(defn generate-default-frame [method]
  "Produce a default aliasing diagram for the given SootMethod"
  (println "!!! Generated default frame for" method "(sig-hash:" (hash (-> method .getSignature)) ")") 
  (let [return-type (-> method .getReturnType)
        returns-void (instance? VoidType return-type)
        returns-primitive (instance? PrimType return-type)]
    (cond 
      returns-void (d/new-diagram [])
      :else (-> (d/new-diagram [])
              (d/add-object [d/ANY-OBJ])
              (d/add-return-val d/ANY-OBJ)))))

(defn get-frame-from-library [method]
  "Check whether the library has a diagram for the given SootMethod. If so, return it.
   If not, generate a default diagram."
  (let [entry (frame-library (keyword (str (hash (-> method .getSignature)))))]
    (if (= nil entry)
      (generate-default-frame method)
        entry)))