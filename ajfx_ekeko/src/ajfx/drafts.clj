
(ns 
  ^{:doc "Helper functions."
    :author "Tim Molderez" }
  ajfx.drafts
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

(defn bla []
  2)
 
  
(comment 
(defn 
  soot|unit|ajcwrites-soot|field
  [?unit ?field]
  (l/fresh [?expr ?method ?fieldname]
  (soot|unit|invocation-soot|value|invocation ?unit ?expr)
  (soot|value|invocation-soot|method ?expr ?method)
  (soot|method|ajcfield|set-fieldname ?method ?fieldname)
  (soot|field-name ?field ?fieldname)))



(damp.ekeko/ekeko* [?advice ?type] (w/advice ?advice)
                      (equals ?type (.getClass ?advice)))



; Fetch all JInvokeStmts inside advice
(damp.ekeko/ekeko* [?advice ?unit]
(l/fresh [?method]
         (ajsoot/advice-soot|method ?advice ?method)
         (jsoot/soot|method-soot|unit ?method ?unit)))
  
  
  
(damp.ekeko/ekeko [?unit ?value]
(l/fresh [?soot|method ?advice ?usebox]
         (ajsoot/advice-soot|method ?advice ?soot|method)
         (jsoot/soot|method-soot|unit ?soot|method ?unit)
         
         (jsoot/soot-unit :JInvokeStmt ?unit)
        ; (succeeds (instance? soot.jimple.internal.JInvokeStmt ?unit))
         
     ;    (jsoot/soot-unit-usebox ?unit ?usebox)
      ;   (jsoot/soot-valuebox-value ?usebox ?value)
         
         ))


; Fetch all aspects
  (damp.ekeko/ekeko* [?aspect] (w/aspect ?aspect))


; Relate aspects to the fields they modify
  (damp.ekeko/ekeko* [?aspect ?field]
                   (l/fresh [?advice]
                          (aspect-advice ?aspect ?advice)
                          (ajsoot/advice|writes-field ?unit ?value)))

(damp.ekeko/ekeko* [?advice ?field]
                          (ajsoot/advice|writes-field ?advice ?field))



(damp.ekeko/ekeko* [?aspect ?field]
                      (l/fresh [?advice]
                             (w/aspect-advice ?aspect ?advice)
                             (ajsoot/advice|writes-field ?advice ?field)))

(damp.ekeko/ekeko* [?advice ?soot|unit]
(l/fresh [?soot|method]
         (ajsoot/advice-soot|method ?advice ?soot|method)
         (jsoot/soot|method-soot|unit ?soot|method ?soot|unit)))

; Get the type of an object..
(damp.ekeko/ekeko* [?adv ?field ?type]
                   (advice|fieldWrite-soot|field ?adv ?field)
                    (equals ?type (.getClass ?field)))

(inspect-tree 
  (damp.ekeko/ekeko
    [a b]
    (advice|methodCall-soot|method a b)))
)