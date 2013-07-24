(ns 
  ^{:doc "Extra relations to query Soot control-flow graph."
    :author "Tim Molderez" }
  ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l] )
  (:use [clojure.inspector])
  (:use [damp.ekeko logic])
  (:use [damp.ekeko])
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
  [?advice ?container ?method]
  "Relates an advice to the methods it calls (directly in the advice body)"
  (l/fresh [?ajcCall ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (l/conde [
                     (soot|unit|invocation-soot|value|invocation ?unit ?value)
                     (soot|value|invocation-soot|method ?value ?ajcCall)
                     (soot|method|ajccall-soot|method ?ajcCall ?method)]
                    [
                     (jsoot/soot-unit-calls-method ?unit ?method)])
           (equals ?container (.getValue(first (.getUseBoxes ?unit))))))


(defn 
  method-methodCalls
  [?advice ?container ?method]
  "Relates a method to the method calls it contains"
  (l/fresh [?unit]
           (jsoot/soot|method-soot|unit ?method ?unit)
           (jsoot/soot-unit-calls-method ?unit ?method)
           (equals ?container (.getValue(first (.getUseBoxes ?unit))))))

(defn varType
  [?var ?method ?kind]
  "Determine what kind of a variable ?var is
@param ?var    the variable
@param ?method the variable must appear within this method body (SootMethod)
@param ?kind   a keyword representing the variable's kind (:local, :global, :parameter or :this)"
  (l/fresh [?unit ?lhs]
    (l/conde [
              (jsoot/soot|method-soot|unit ?method ?unit)
              (jsoot/soot-unit :JIdentityStmt ?unit)
              (= (.getName ?var) (.getName (.getLeftOp ?unit)))
              (l/conde [
                        (= "soot.jimple.ThisRef" (type (.getRightOp ?unit)))
                        (equals ?kind :this)]
                       [
                        (= "soot.jimple.ParameterRef" (type (.getRightOp ?unit)))
                        (equals ?kind :parameter)]
                       [
                        (equals ?kind :local)])
              ]
             [
              (equals ?kind :global)])
    
    
  ;(jsoot/soot|method-soot|unit ?method)
  
  )
)

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
    [directWrites (damp.ekeko/ekeko [?container ?field]
                    (methodFieldSet-container-field method ?container ?field))
     directCalls (damp.ekeko/ekeko [?container ?method]
                    (method-methodCalls method ?container ?method))]
    directWrites))


(defn pullUpFrame
  "Suppose we wish to infer the frame condition of a method A, and its body contains
a call to method B. We've inferred the frame condition of B, but we still need to rephrase it
so it can be added to A's frame condition, which is what this function does. In other words, 
this function 'pulls up' the frame
condition of B to A."
  [call ]
  )

(defn inferAdviceFrame
  [advice]
  "Infer the frame condition of an advice.
@param advice the advice we want to analyse (can be obtained via w/advice)
@return the frame condition is a list of variables that might change
@see inferMethodFrame"
  (let 
    [directWrites (damp.ekeko/ekeko [?container ?field]
                    (adviceFieldSet-container-field advice ?container ?field))
     directCalls (damp.ekeko/ekeko [?container ?method]
                    (advice|methodCall-soot|method advice ?container ?method))
     indirectWrites (for [x directCalls]
                      (inferMethodFrame (nth x 1)))]
    indirectWrites))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scratch pad ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(inspect-tree(let [allAdvice (damp.ekeko/ekeko [?advice] (w/advice ?advice))]
     (inferAdviceFrame (first(first allAdvice)))))

(inspect-tree 
  (damp.ekeko/ekeko
    [b]
    (l/fresh [a]
             (jsoot/soot|method-soot|unit a b)
             (soot|method-name a "helperMethod"))))

(inspect-tree 
     (damp.ekeko/ekeko [?a ?b ?c ?d]
                       (l/fresh []
                       (virtMethodCall-receiver ?a ?b)
                       (jsoot/soot|method-soot|unit ?c ?b)
                       (jsoot/soot-method-units ?c ?d)
                       ;(equals ?d (.getSignature ?c)
                               )))

(inspect-tree 
     (damp.ekeko/ekeko [?b ?c ?d ?e]
                       (l/fresh [?a]
                       (advice-soot|unit ?a ?b)
                       (equals ?c (.getUseBoxes ?b))
                       (equals ?d (type ?b))
                       (jsoot/soot-unit :JIdentityStmt ?b)
                       (equals ?e (.getLeftOp ?b))
                               )))


(comment
  (defn inferAdviceFrame
  [advice]
  (damp.ekeko/ekeko [?field]
                    (advice|field|set-soot|field advice ?field)
                    ))
  
  (inspect-tree (damp.ekeko/ekeko
  [a b]
  (fieldAssignmentUnit-fieldContainer a b)
  ))
  
  (damp.ekeko/ekeko*
     [a b c]
     (advice-soot|unit a b)
     (jsoot/soot-unit c b))
  
  ; Using inferAdviceFrame
  (let [allAdvice (damp.ekeko/ekeko [?advice] (w/advice ?advice))]
     (.getClass (first allAdvice)))
  
  (let [allAdvice (damp.ekeko/ekeko [?advice] (w/advice ?advice))]
     (inferAdviceFrame (first(first allAdvice))))
  
  (let [allAdvice (damp.ekeko/ekeko [?advice] (w/advice ?advice))]
     (map first allAdvice))
  
  
  
(let [x [1 2 3]]
  (l/run* [q]
      (l/membero q x)
      (l/membero q [2 3 4])))

(defn 
  soot-unit-calls-method2
  [?unit ?m] 
  (l/fresh [?model ?keyw ?scene ?methods]
         (jsoot/soot-model-scene ?model ?scene)
         (jsoot/soot-unit ?keyw ?unit)
         (equals ?methods (iterator-seq (.dynamicUnitCallees ^SootProjectModel ?model ?unit)))
         (contains ?methods ?m)
         (jsoot/soot :method ?m) ;application methods only
         ))

(defn 
  advice|methodCall-soot|method
  [?advice ?method ?method2]
  (l/fresh [?unit ]
           ;(advice-soot|unit ?advice ?unit)
           (ajsoot/advice-soot|method ?advice ?method2)
           (jsoot/soot|method-soot|unit ?method2 ?unit)
           (jsoot/soot-unit-calls-method ?unit ?method)
           ;(equals ?u1 (.hashCode ?unit))
           ;(equals ?u2 (.hashCode ?unit1))
           ;(equals ?u1 ?u2)
           ))

(defn 
  advice|methodCall-soot|method
  [?advice ?method]
  (l/fresh [?unit ?advAsMethod]
           (ajsoot/advice-soot|method ?advice ?advAsMethod)
           (jsoot/soot-method-called-by-method ?method ?advAsMethod)))

)