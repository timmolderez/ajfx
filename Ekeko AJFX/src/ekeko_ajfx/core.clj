(ns 
  ^{:doc "Use AJFX to infer the frame conditions of Java/AspectJ code."
    :author "Tim Molderez" }
  ekeko-ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use [inspector-jay.core]
        [clojure.repl]
        [damp.ekeko logic]
        [damp.ekeko]
        [ekeko-ajfx.soot]
        [ekeko-ajfx.analysis])
  (:import [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph LoopNestTree]))

(defn do-analysis [method]
  (ekeko-ajfx.diagram/reset-obj-id)
  (-> ekeko-ajfx.analysis/started-analysis .clear)
  (ekeko-ajfx.analysis/clear-cache)
  (ekeko-ajfx.analysis/infer-frame method))

(defn get-all-advice []
  (set (ekeko [?adv]
         (w/advice ?adv))))

(defn get-all-advice-and-shadows []
  (set (ekeko [?adv ?meth]
         (w/advice ?adv)
         (w/advice-shadow ?adv ?meth))))

(defn get-all-advice-bodies []
  (set (ekeko [?method]
         (l/fresh [?advice]
           (w/advice ?advice)
           (ajsoot/advice-soot|method ?advice ?method)))))

(defn get-all-bodies []
  (ekeko [?method]
         (l/fresh []
           (jsoot/soot :method ?method))))

(defn get-method-from-shadow [shadow]
  (let [str (-> shadow .toString)
        open (-> str (.indexOf "("))
        close (-> str (.lastIndexOf ")"))
        ?sig (subs str (inc open) close)
        query (ekeko [?m] (ekeko-ajfx.soot/soot|method-sig ?m ?sig))]
    ((first query))))

(comment

  (inspect (new java.io.File "."))
  (inspect (get-all-advice))
  (inspect (get-method-from-shadow 
             (nth (for [x (get-all-advice-and-shadows)] (second x)) 6)))
  (inspect (get-all-advice-bodies))
  (count (get-all-advice-bodies))
  (inspect (get-all-bodies))
  
  (let [advice (nth (for [x (get-all-advice-bodies)] (first x)) 10)]
    (time (do-analysis advice)))
  
  (inspect (nth (for [x (get-all-advice-bodies)] (first x)) 9))
  
  (inspect (filter
             (fn [x]
               (-> (first x) .getName (.startsWith "ajc$")))
             (get-all-bodies)))
  
  (inspect (for [x (get-all-advice)]
            (-> (first x) .getSourceLocation)))
  
  (inspect
    (let [q (ekeko [?a] (soot|method-name ?a "newShip"))
          method (first (first q))]
      method))
  )

;(defn advice []
;  (ek/ekeko [?advice]
;            (wea/advice 
;              ?advice)))
;
;(defn shadow []
;  (ek/ekeko [?shadow]
;            (wea/shadow 
;              ?shadow)))
;
;(defn advice-shadow []
;  (ek/ekeko [?advice ?shadow]
;            (wea/advice-shadow 
;              ?advice 
;              ?shadow)))
;
;(defn method []
;  (ek/ekeko [?method]
;            (wea/method 
;              ?method)))
;
;(defn shadow|invocation-method|called
;  "Returns the BcelMethod referenced within the provided shadow"
;  ([] (map 
;        (fn[x](vector 
;                (first x) 
;                (shadow|invocation-method|called (first x)))) 
;        (shadow)))
;  ([shadow] (first(first(filter 
;                          (fn [x] (.contains (.toString shadow) (.toString (first x)))) 
;                          (method))))))
;
;(defn bceladvice|aspect [advice]
;  (ek/ekeko [?aspect]
;            (wea/aspect-advice ?aspect advice)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scratch pad ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Get the units of a particular method
;(inspect
;  (damp.ekeko/ekeko
;    [?unit]
;    (jsoot/soot-unit :JIdentityStmt ?unit)))

; Find the calls in a particular method
(inspect 
  (ekeko [?b]
    (l/fresh [?a ?callee ?recv]
             (jsoot/soot|method-soot|unit ?a ?b)
             (soot|method-name ?a "helperMethod")
             (method-methodCalls ?a ?callee ?b ?recv))))

;;;;;;;;;;;;;;;
(ekeko-ajfx.diagram/reset-obj-id)
(-> ekeko-ajfx.analysis/started-analysis .clear)
(let [q (ekeko [?a] (soot|method-name ?a "getSize"))
      method (first (first q))
      frame (infer-frame method)]
  frame)
;;;;;;;;;;;;;;


(inspect (clojure.stacktrace/root-cause *e))

(inspect
  (let [q (ekeko [?a] (soot|method-name ?a "ajc$perObjectBind"))
        method (first (first q))]
    (-> method .getActiveBody .getUnits)))


(inspect
  (let [q (ekeko [?a] (soot|method-name ?a "helper3"))
        method (first (first q))]
    method))

(ekeko [?a ?b] (soot|method-name ?a ?b))

(inspect
  (let [q (ekeko [?a] (soot|method-name ?a "helper"))
        method (first (first q))]
    (new LoopNestTree (-> method .getActiveBody))))

(let [q (ekeko [?a] (soot|method-name ?a "helper"))
        method (first (first q))
        body (-> method .getActiveBody)]
    (showBlockCFG body))

(inspect 
  (ekeko
    [?b ?a]
    (l/fresh []
             (jsoot/soot|method-soot|unit ?a ?b)
             (soot|method-name ?a "helper")
             )))

(inspect 
  (ekeko
    [?b ?c]
    (l/fresh [?a]
             (jsoot/soot|method-soot|unit ?a ?b)
             (jsoot/soot-unit :JAssignStmt ?b)
             (soot|method-name ?a "start")
             (equals ?c (.getLeftOp ?b))
             )))

(inspect 
     (ekeko [?a ?b ?c ?d]
                       (l/fresh []
                       (virtMethodCall-receiver ?a ?b)
                       (jsoot/soot|method-soot|unit ?c ?b)
                       (jsoot/soot-method-units ?c ?d)
                       ;(equals ?d (.getSignature ?c)
                               )))

(inspect 
     (ekeko [?b ?c ?d ?e]
                       (l/fresh [?a]
                       (advice-soot|unit ?a ?b)
                       (equals ?c (.getUseBoxes ?b))
                       (equals ?d (type ?b))
                       (jsoot/soot-unit :JIdentityStmt ?b)
                       (equals ?e (.getLeftOp ?b))
                               )))
)