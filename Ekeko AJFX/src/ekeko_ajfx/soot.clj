(ns
  ^{:doc "Extra relations to query Soot control-flow graph."
    :author "Tim Molderez" }
  ekeko-ajfx.soot
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

(defn- 
   lastIndexOfText 
   [from to]  
   (> (.lastIndexOf from to) -1))

 (defn-  
   IndexOfText  
   [from to](> (.indexOf from to) -1))

;(inspect-tree
;(damp.ekeko/ekeko 
;  [?a ?b]
;  (l/fresh []
;           (w/aspect-advice ?a ?b)
;           (equals "bank.aspects.Security" (-> ?a .getName))
;           (w/advice|before ?b))))
;
;(inspect-tree (ekeko [?a ?c] 
;                     (w/aspect-shadow ?a ?c)))
;
;(inspect-tree (ekeko [?b ?a]
;   (l/fresh []
;       (advice-soot|unit ?a ?b))))

;(inspect
;(damp.ekeko/ekeko [?a ?b]
;           (w/advice ?a)
;           (ajsoot/advice-soot|method ?a ?b)))

(count (ekeko [?a] (w/advice ?a)))


(defn get-class-name
  "Retrieve the (absolute) class name of an object
@param obj  a Java object
@return     absolute class name"
  [obj]
  (.getName (.getClass obj)))

(defn
  advice-soot|unit
  [?advice ?unit]
  "Relates an advice to the soot units its body contains"
  (l/fresh [?method]
           (ajsoot/advice-soot|method ?advice ?method)
           (jsoot/soot|method-soot|unit ?method ?unit)))

(defn
  soot|unit|invocation
  "Is ?unit a JInvokeStmt?"
  [?unit]
  (l/all
    (jsoot/soot-unit :JInvokeStmt ?unit)))

(defn-
  soot|unit|invocation-soot|value|invocation
  "Relation between ?unit and the invoke expression it contains"
  [?unit ?value]
  (l/all
    (soot|unit|invocation ?unit)
    (equals ?value (.getInvokeExpr ?unit)
              )))

(defn
  soot|value|invocation-soot|method
  "Relate a invoke expression to the method being called"
  [?value ?method]
  (l/fresh [?unit]
           (soot|unit|invocation-soot|value|invocation ?unit ?value)
           (equals ?method (.getMethod ?value))
    ))

(defn-
  soot|method|static
  "Is this soot method static?"
  [?method]  
  (l/all
    (jsoot/soot :method ?method)
    (succeeds (.isStatic ?method))
  ))

(defn 
  soot|method-name
  "Relate a soot method to its name"
  [?method ?name]
  (l/all
    (jsoot/soot :method ?method)
    (equals ?name (.getName ?method))))

(defn 
  soot|field-name
  "Relate a soot field to its name"
  [?field ?name]
  (l/all
    (jsoot/soot :field ?field)
    (equals ?name (.getName ?field))))

(defn- 
  soot|method|ajcfield|get-soot|field
  "Relate a soot invoke expression representing an ajc field read to the corresponding soot field.
(It seems such reads are only produced when accessing stuff within your 'package scope'..)"
  [?method ?field]
  (l/fresh [?methodName ?split ?fieldName ?fieldType]
           ; Find ?field based on name
           (soot|method-name ?method ?methodName)
           (equals ?split (clojure.string/split ?methodName #"\$")) 
           (equals "ajc" (nth ?split 0))
           (equals "inlineAccessFieldGet" (nth ?split 1) )
           (equals ?fieldName (last ?split))
           (soot|field-name ?field ?fieldName)
           
           ; Find ?field based on class containing the field
           (equals ?fieldType (.getType(.getDeclaringClass ?field)))
           (equals ?fieldType (.getParameterType ?method 0))))

(defn- 
  soot|method|ajcfield|set-soot|field
  "Relate a soot invoke expression representing an ajc field write to the corresponding soot field.
(It seems such writes are only produced when modifying stuff within your 'package scope'..)"
  [?method ?field]
  (l/fresh [?methodName ?split ?fieldName ?fieldType]
           ; Find ?field based on its name
           (soot|method-name ?method ?methodName)
           (equals ?split (clojure.string/split ?methodName #"\$")) 
           (equals "ajc" (nth ?split 0))
           (equals "inlineAccessFieldSet" (nth ?split 1) )
           (equals ?fieldName (last ?split))
           (soot|field-name ?field ?fieldName)
           
           ; Find ?field based on class containing the field
           (equals ?fieldType (.getType(.getDeclaringClass ?field)))
           (equals ?fieldType (.getParameterType ?method 0))
           ))

(defn-
  soot|method|ajccall-soot|method
  "Relate an ajc$invoke method to the soot method it calls"
  [?ajcMethod ?sootMethod]
  (l/fresh [?ajcname ?split ?mName ?mType]
           (soot|method-name ?ajcMethod ?ajcname)
           (equals ?split (clojure.string/split ?ajcname #"\$")) 
           (equals "ajc" (nth ?split 0))
           (equals "inlineAccessMethod" (nth ?split 1) )
           (equals ?mName (last ?split))
           (soot|method-name ?sootMethod ?mName)
           
           (equals ?mType (.getType(.getDeclaringClass ?sootMethod)))
           (equals ?mType (.getParameterType ?ajcMethod 0))))

; Go fetch the container in an inlineAccessFieldSet unit (i.e. the foo in foo.bar=baz;)
(defn
  fieldAssignmentUnit-fieldContainer
  [?val ?unit ]
  (l/fresh [?meth  ?methodName ?split]
    (soot|value|invocation-soot|method ?unit ?meth)
    
    
    (soot|method-name ?meth ?methodName)
    (equals ?split (clojure.string/split ?methodName #"\$")) 
    (equals "ajc" (nth ?split 0))
    (equals "inlineAccessFieldSet" (nth ?split 1) )
    (equals ?val (.getValue(nth (.getUseBoxes ?unit) 0)))))


(defn
  methodCall-receiver
  [?val ?unit]
  (l/fresh [?meth  ?methodName ?split]
    (soot|value|invocation-soot|method ?unit ?meth)
    (soot|method-name ?meth ?methodName)
    (equals ?split (clojure.string/split ?methodName #"\$")) 
    (equals "ajc" (nth ?split 0))
    (equals "inlineAccessMethod" (nth ?split 1) )
    (equals ?val (.getValue(nth (.getUseBoxes ?unit) 0)))
           ))

(defn
  virtMethodCall-receiver
  [?val ?unit]
  (l/fresh [?adv ?expr]
    (advice-soot|unit ?adv ?unit)
    (jsoot/soot-unit :JInvokeStmt ?unit)
    (equals ?expr (.getInvokeExpr ?unit))
    (equals "JVirtualInvokeExpr" (.getSimpleName(.getClass ?expr)))
    (equals ?val (.getBase ?expr))
           ))

(defn 
  advice|field|get-soot|field
  [?advice ?field]
  "Relates an advice to the fields it modifies (directly in the advice body)"
  (l/fresh [?getMethod ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (l/conde [
                     (soot|unit|invocation-soot|value|invocation ?unit ?value)
                     (soot|value|invocation-soot|method ?value ?getMethod)
                     (soot|method|ajcfield|get-soot|field ?getMethod ?field)]
                    [
                     (jsoot/soot|unit|reads-soot|field ?unit ?field)])))

(defn 
  adviceFieldSet-container-field
  [?advice ?container ?field]
  "Relates an advice to its field assignment statements
TODO if assignment to static field, set the container field to the class name 
@param ?advice BcelAdvice      the advice
@param ?container JimpleLocal  the instance of which the field is modified
@param ?field SootField        the field being modified"       
  (l/fresh [?setMethod ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (l/conde [
                     ; In case of an inlineAccessFieldSet
                     (soot|unit|invocation-soot|value|invocation ?unit ?value)
                     (soot|value|invocation-soot|method ?value ?setMethod)
                     (soot|method|ajcfield|set-soot|field ?setMethod ?field)
                     ]
                    [
                     ; A regular field assignment
                     (jsoot/soot|unit|writes-soot|field ?unit ?field)
                     ])
           (equals ?container (.getValue(first (.getUseBoxes ?unit))))))

(defn 
  methodFieldSet-container-field
  [?method ?container ?field]
  "Relates a method to any field assignments in its body 
@param ?advice SootMethod      the method
@param ?container JimpleLocal  the instance of which the field is modified 
@param ?field SootField        the field being modified"       
  (l/fresh [?unit]
           (jsoot/soot|method-soot|unit ?method ?unit)
           (jsoot/soot|unit|writes-soot|field ?unit ?field)
                     
           (equals ?container (.getValue(first (.getUseBoxes ?unit))))))

(defn 
  method-methodCalls
  [?caller ?callee ?call ?receiver]
  "Relates a method to the method calls it contains"
  (l/fresh []
           (jsoot/soot|method-soot|unit ?caller ?call)
           (jsoot/soot-unit-calls-method ?call ?callee)
           (equals ?receiver (.getValue(first (.getUseBoxes ?call))))
           (succeeds (instance? soot.jimple.InvokeStmt ?call))
           (fails (equals soot.jimple.internal.JSpecialInvokeExpr (-> ?call .getInvokeExpr .getClass)))))

(defn 
  advice|methodCall-soot|method
  [?caller ?callee ?call ?receiver]
  "Relates an advice to the methods it calls (directly in the advice body)"
  (l/fresh [?ajcCall ?value ?method]
    (ajsoot/advice-soot|method ?caller ?method)
    (jsoot/soot|method-soot|unit ?method ?call)
    (l/conde 
      [(soot|unit|invocation-soot|value|invocation ?call ?value)
       (soot|value|invocation-soot|method ?value ?ajcCall)
       (soot|method|ajccall-soot|method ?ajcCall ?callee)
       (equals ?receiver (.getValue(first (.getUseBoxes ?call))))]
      [(method-methodCalls ?method ?callee ?call ?receiver)])))