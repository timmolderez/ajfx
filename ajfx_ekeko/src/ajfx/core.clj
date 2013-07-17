(ns 
  ^{:doc "Extra relations to query Soot control-flow graph."
    :author "Tim Molderez" }
  ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l] )
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

(defn-
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
  advice|field|set-soot|field
  [?advice ?field]
  "Relates an advice to the fields it modifies (directly in the advice body)"
  (l/fresh [?setMethod ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (l/conde [
                     (soot|unit|invocation-soot|value|invocation ?unit ?value)
                     (soot|value|invocation-soot|method ?value ?setMethod)
                     (soot|method|ajcfield|set-soot|field ?setMethod ?field)]
                    [
                     (jsoot/soot|unit|writes-soot|field ?unit ?field)])))

(defn 
  advice|methodCall-soot|method
  [?advice ?method]
  "Relates an advice to the fields it modifies (directly in the advice body)"
  (l/fresh [?ajcCall ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (l/conde [
                     (soot|unit|invocation-soot|value|invocation ?unit ?value)
                     (soot|value|invocation-soot|method ?value ?ajcCall)
                     (soot|method|ajccall-soot|method ?ajcCall ?method)]
                    [
                     (jsoot/soot-unit-calls-method ?unit ?method)])))

(defn inferAdviceFrame
  [advice]
  "Infer the frame condition of an advice"
  (damp.ekeko/ekeko [?field]
                    (advice|field|set-soot|field advice ?field)
                    ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scratch pad ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  
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

(defn test2
  [x]
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