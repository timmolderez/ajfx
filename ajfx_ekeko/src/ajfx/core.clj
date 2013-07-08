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

;; Sandbox to tinker with GASR..
;; @author Tim

(comment
(damp.ekeko/ekeko* [?aspect ?field]
                      (l/fresh [?advice]
                             (w/aspect-advice ?aspect ?advice)
                             (ajsoot/advice|writes-field ?advice ?field)))

(damp.ekeko/ekeko* [?advice ?soot|unit]
(l/fresh [?soot|method]
         (ajsoot/advice-soot|method ?advice ?soot|method)
         (jsoot/soot|method-soot|unit ?soot|method ?soot|unit)))
)

(defn 
  soot|unit|invocation
  "Is ?unit a JInvokeStmt?"
  [?unit ]
  (l/all
    (jsoot/soot-unit :JInvokeStmt ?unit)
    ))

(defn 
  soot|unit|invocation-soot|value|invocation
  "Relation between ?unit and the invoke expression it contains"
  [?unit ?value]
  (l/all
    (soot|unit|invocation ?unit)
    (equals ?value (.getInvokeExpr ?unit)
              )))


(defn
  soot|value|invocation-soot|method
  [?value ?method]
  (l/fresh [?unit]
           (soot|unit|invocation-soot|value|invocation ?unit ?value)
           (equals ?method (.getMethod ?value))
    ))


(defn 
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


(comment
(defn 
  soot|unit|ajcwrites-soot|field
  [?unit ?field]
  (l/fresh [?expr ?method ?fieldname]
  (soot|unit|invocation-soot|value|invocation ?unit ?expr)
  (soot|value|invocation-soot|method ?expr ?method)
  (soot|method|ajcfield|set-fieldname ?method ?fieldname)
  (soot|field-name ?field ?fieldname))))







; Fetch all JInvokeStmts inside advice

(damp.ekeko/ekeko* [?advice ?unit]
(l/fresh [?method]
         (ajsoot/advice-soot|method ?advice ?method)
         (jsoot/soot|method-soot|unit ?method ?unit)))

(comment
(damp.ekeko/ekeko [?unit ?value]
(l/fresh [?soot|method ?advice ?usebox]
         (ajsoot/advice-soot|method ?advice ?soot|method)
         (jsoot/soot|method-soot|unit ?soot|method ?unit)
         
         (jsoot/soot-unit :JInvokeStmt ?unit)
        ; (succeeds (instance? soot.jimple.internal.JInvokeStmt ?unit))
         
     ;    (jsoot/soot-unit-usebox ?unit ?usebox)
      ;   (jsoot/soot-valuebox-value ?usebox ?value)
         
         )))


; Fetch all aspects
(comment
  (damp.ekeko/ekeko* [?aspect] (w/aspect ?aspect))

; Relate aspects to the fields they modify
  (damp.ekeko/ekeko* [?aspect ?field]
                   (l/fresh [?advice]
                          (aspect-advice ?aspect ?advice)
                          (ajsoot/advice|writes-field ?advice ?field)))

(damp.ekeko/ekeko* [?advice ?field]
                          (ajsoot/advice|writes-field ?advice ?field))



)