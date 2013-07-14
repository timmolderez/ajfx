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
     [soot :as jsoot]]))

(defn advice-soot|unit
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

(defn soot|method|ajcfield|set-soot|field
  "Relate a soot method to the field it represents"
  [?method ?field]
  (l/fresh [?methodname ?split ?fieldname]
           (soot|method-name ?method ?methodname)
           (equals ?split (clojure.string/split ?methodname #"\$")) 
           (equals "ajc" (nth ?split 0))
           (equals "inlineAccessFieldSet" (nth ?split 1) )
           (equals ?fieldname (last ?split))
           (soot|field-name ?field ?fieldname)
  ))

(defn- soot|method|ajcfield|set-soot|field
  "Relate a soot method representing a field write to the corresponding soot field"
  [?method ?field]
  (l/fresh [?methodname ?split ?fieldname]
           (soot|method-name ?method ?methodname)
           (equals ?split (clojure.string/split ?methodname #"\$")) 
           (equals "ajc" (nth ?split 0))
           (equals "inlineAccessFieldSet" (nth ?split 1) )
           (equals ?fieldname (last ?split))
           (soot|field-name ?field ?fieldname)
  ))

(defn- soot|method|ajcfield|get-soot|field
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
           
           ; Find ?field based on type (which can be found in the first parameter of the invoke expression)
           (equals ?fieldType (.getType(.getDeclaringClass ?field)))
           (equals ?fieldType (.getParameterType ?method 0))))

(defn- soot|method|ajcfield|set-soot|field
  "Relate a soot invoke expression representing an ajc field write to the corresponding soot field.
(It seems such writes are only produced when modifying stuff within your 'package scope'..)"
  [?method ?field]
  (l/fresh [?methodName ?split ?fieldName ?fieldType]
           ; Find ?field based on name
           (soot|method-name ?method ?methodName)
           (equals ?split (clojure.string/split ?methodName #"\$")) 
           (equals "ajc" (nth ?split 0))
           (equals "inlineAccessFieldSet" (nth ?split 1) )
           (equals ?fieldName (last ?split))
           (soot|field-name ?field ?fieldName)
           
           ; Find ?field based on type (which can be found in the second parameter of the invoke expression)
           (equals ?fieldType (.getType(.getDeclaringClass ?field)))
           (equals ?fieldType (.getParameterType ?method 1))))

(defn advice|ajcfield|get-soot|field
  [?advice ?field]
  "Relates an advice to the fields it modifies (directly in the advice body)"
  (l/fresh [?getMethod ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (soot|unit|invocation-soot|value|invocation ?unit ?value)
           (soot|value|invocation-soot|method ?value ?getMethod)
           (soot|method|ajcfield|get-soot|field ?getMethod ?field)))

(defn advice|ajcfield|set-soot|field
  [?advice ?field]
  "Relates an advice to the fields it modifies (directly in the advice body)"
  (l/fresh [?setMethod ?value ?unit]
           (w/advice ?advice)
           (advice-soot|unit ?advice ?unit)
           (soot|unit|invocation-soot|value|invocation ?unit ?value)
           (soot|value|invocation-soot|method ?value ?setMethod)
           (soot|method|ajcfield|set-soot|field ?setMethod ?field)))