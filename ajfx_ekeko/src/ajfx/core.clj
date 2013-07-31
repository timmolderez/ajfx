(ns 
  ^{:doc "Extra relations to query Soot control-flow graph."
    :author "Tim Molderez" }
  ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l] )
  (:use [clojure.inspector])
  (:use [damp.ekeko logic])
  (:use [damp.ekeko])
  (:use [ajfx.soot-ext])
  (:require 
    [damp.ekeko.aspectj
     [weaverworld :as w]
     [soot :as ajsoot]
            ;[ajdt :as ajdt] 
             ;[xcut :as xcut]
            ]
    [damp.ekeko.soot
     [soot :as jsoot]])
  (:import 
    [java.lang System]))

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
    (jsoot/soot-unit :JInvokeStmt ?unit)
    ))

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
    (equals ?val (.getValue(nth (.getUseBoxes ?unit) 0)))
              ))


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
  advice|methodCall-soot|method
  [?caller ?callee ?call ?receiver]
  "Relates an advice to the methods it calls (directly in the advice body)"
  (l/fresh [?ajcCall ?value ?unit]
           (w/advice ?caller)
           (advice-soot|unit ?caller ?call)
           (l/conde [
                     (soot|unit|invocation-soot|value|invocation ?call ?value)
                     (soot|value|invocation-soot|method ?value ?ajcCall)
                     (soot|method|ajccall-soot|method ?ajcCall ?callee)]
                    [
                     (jsoot/soot-unit-calls-method ?call ?callee)])
           (equals ?receiver (.getValue(first (.getUseBoxes ?call))))))


(defn 
  method-methodCalls
  [?caller ?callee ?call ?receiver]
  "Relates a method to the method calls it contains"
  (l/fresh []
           (jsoot/soot|method-soot|unit ?caller ?call)
           (jsoot/soot-unit-calls-method ?call ?callee)
           (equals ?receiver (.getValue(first (.getUseBoxes ?call))))))

(defn varType
  [?var ?method]
  "Determine what kind of a variable ?var is
@param ?var    the variable (JimpleLocal)
@param ?method the variable must appear within this method body (SootMethod)
@return a keyword representing the variable's kind (:local, :global, :parameter or :this)"
  (first (ekeko [?kind]
    (l/fresh [?unit ?lhs]
      (l/conde [
                (jsoot/soot|method-soot|unit ?method ?unit)
                (jsoot/soot-unit :JIdentityStmt ?unit)
                (= (.getName ?var) (.getName (.getLeftOp ?unit)))
                (l/conde [
                          (= "soot.jimple.ThisRef" (.getClass (.getRightOp ?unit)))
                          (equals ?kind :this)]
                         [
                          (= "soot.jimple.ParameterRef" (.getClass (.getRightOp ?unit)))
                          (equals ?kind :parameter)]
                         [
                          (equals ?kind :local)])
                ]
               [
                (equals ?kind :global)])))))

(defn inferMethodFrame
  [method]
  "Infer the frame condition of a method.
@param method the SootMethod we want to analyse
@return the frame condition is a list of variables that might change, such that this list can be understood by the advice itself
For example, the function could return a list like this:
[
  [aCar, SootField<wheel>],                             ; refers to aCar.wheel , where aCar is a parameter of the advice
  [aTruck, SootField<cargo>, SootField<contents>],      ; aTruck.cargo.contents
  [this, SootField<cargo>]                              ; this.cargo
]"
    (let 
    [directWrites (ekeko [?container ?field]
                    (methodFieldSet-container-field method ?container ?field))
     directCalls (ekeko [?container ?method]
                    (method-methodCalls method ?container ?method))]
    directWrites))

(defn get-class-name
  "Retrieve the (absolute) class name of an object
@param obj  a Java object
@return     absolute class name"
  [obj]
  (.getName (.getClass obj)))


(defn jimpleLocal-parameterIndex
  "Relate the use of a parameter within a method to its index
@param ?local    a JimpleLocal representing a parameter
@param ?method   the JimpleLocal is used within this method
@param ?index    the index of the parameter within the method's interface"
  [?local ?method ?index]
  (l/fresh [?unit]
           (jsoot/soot|method-soot|unit ?method ?unit)
           (jsoot/soot-unit :JIdentityStmt ?unit)
           (equals ?local (.getName (.getLeftOp ?unit)))     
           (equals "soot.jimple.ParameterRef" (get-class-name (.getRightOp ?unit)))
           (equals ?index (.getIndex(.getRightOp ?unit)))
      ))

(defn pullUpFrame
  "Suppose we wish to infer the frame condition of a method a(), and its body contains
a call to method b(). We've inferred the frame condition of B, but we still need to rephrase it
so it can be added to A's frame condition, which is what this function does. In other words, 
this function 'pulls up' the frame
condition of b() to a().
@param call  the call to b() from a() 
@param body  the method body of b()
@param frame the frame axiom of b()
@return  the frame of b() pulled up to the context of a()"
  [call body frame]
  (for [x frame]
    (let [container (nth frame 0)]
      (case (varType container body)
        :local ()
        :global ()
        :param (
                 (jimpleLocal-parameterIndex container )
                 )
        :this()))))



(defn internalLocal-sourceValue
  "Relate a local that was generated by Jimple to the value that it represents in the Java source code"
  []
  )
  
  

(defn inferAdviceFrame
  [advice]
  "Infer the frame condition of an advice.
@param advice the advice we want to analyse (can be obtained via w/advice)
@return the frame condition is a list of variables that might change
@see inferMethodFrame"
  (let 
    [directWrites (ekeko [?container ?field]
                    (adviceFieldSet-container-field advice ?container ?field))
     directCalls (ekeko [?callee ?call ?receiver]
                    (advice|methodCall-soot|method advice ?callee ?call ?receiver))
     callFrames (for [x directCalls]
                      (inferMethodFrame (nth x 1)))
     pulledUpWrites (map pullUpFrame
                         (for [x directCalls] (nth x 2))
                         (for [x directCalls] (nth x 1))
                         callFrames)]
    pulledUpWrites))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scratch pad ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(inspect-tree 
  (ekeko 
    [?a ?b ?c]
    (jimpleLocal-parameterIndex ?a ?b ?c)
    ))

(inspect-tree 
  (ekeko 
    [?a ?b ?c ?d ?e]
    (method-methodCalls ?a ?b ?c ?d)
    (equals ?e (.getUseBoxes ?c))))

(inspect-tree
  (ekeko [?a ?b ?c]
                    (jsoot/soot|method-soot|unit ?a ?b)
                    (soot|method-name ?a "helperMethod")
                    (jsoot/soot-unit-calls-method ?b ?c)
                    ))

(inspect-tree(let [allAdvice (ekeko [?advice] (w/advice ?advice))]
     (inferAdviceFrame (first(first allAdvice)))))

; Get the units of a particular method

(inspect-tree 
  (ekeko
    [?b ?c]
    (l/fresh [?a]
             (jsoot/soot|method-soot|unit ?a ?b)
             (soot|method-name ?a "helperMethod")
             (equals ?c (.toString ?b))
             )))

(inspect-tree 
  (ekeko
    [?b ?c]
    (l/fresh [?a]
             (jsoot/soot|method-soot|unit ?a ?b)
             (jsoot/soot-unit :JAssignStmt ?b)
             (soot|method-name ?a "start")
             (equals ?c (.getLeftOp ?b))
             )))

(inspect-tree 
     (ekeko [?a ?b ?c ?d]
                       (l/fresh []
                       (virtMethodCall-receiver ?a ?b)
                       (jsoot/soot|method-soot|unit ?c ?b)
                       (jsoot/soot-method-units ?c ?d)
                       ;(equals ?d (.getSignature ?c)
                               )))

(inspect-tree 
     (ekeko [?b ?c ?d ?e]
                       (l/fresh [?a]
                       (advice-soot|unit ?a ?b)
                       (equals ?c (.getUseBoxes ?b))
                       (equals ?d (type ?b))
                       (jsoot/soot-unit :JIdentityStmt ?b)
                       (equals ?e (.getLeftOp ?b))
                               )))
