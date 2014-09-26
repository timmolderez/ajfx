(ns 
  ^{:doc "Use AJFX to infer the frame conditions of Java/AspectJ code."
    :author "Tim Molderez" }
ekeko-ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
    [ekeko-ajfx.util :as u]
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
    [ekeko-ajfx.analysis]
    )
  (:import [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph LoopNestTree]
    [java.util.concurrent TimeoutException]))

(defn do-analysis [method]
  (ekeko-ajfx.diagram/reset-obj-id)
  (-> ekeko-ajfx.analysis/started-analysis .clear)
  (ekeko-ajfx.analysis/clear-cache)
  (ekeko-ajfx.analysis/infer-frame method))

(defn get-all-advice []
  (set (ekeko [?adv]
         (w/advice ?adv))))

(defn get-all-advice-and-shadows []
  (set (ekeko [?adv-soot ?meth]
         (l/fresh [?adv]
           (w/advice ?adv)
           (w/advice-shadow ?adv ?meth)
           (ajsoot/advice-soot|method ?adv ?adv-soot)))))

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
    (first (first query))))

(defn write-advice-shadows []
  (let [pairs (for [x (get-all-advice-and-shadows)]
                (try
                  [(-> (first x) .getSignature)
                   (-> (get-method-from-shadow (second x)) .getSignature)]
                  (catch Exception e (println "!!! No body found for shadow:" (second x)))))
        filtered (remove (fn [x] (= x nil))
                   pairs)]
    (spit "advice-shadow-pairs.txt" (pr-str (into [] filtered)))))

(defn read-advice-shadows []
  (let [pairs (load-file "advice-shadow-pairs.txt")]
    (into [] (set (for [x pairs]
                  [(first (first (ekeko [?m] (ekeko-ajfx.soot/soot|advice-sig-full ?m (first x)))))
                   (first (first (ekeko [?m] (ekeko-ajfx.soot/soot|method-sig-full ?m (second x)))))])))))

(defn do-complete-analysis []
  (ekeko-ajfx.diagram/reset-obj-id)
  (-> ekeko-ajfx.analysis/started-analysis .clear)
  (ekeko-ajfx.analysis/clear-cache)
  (let [pairs (read-advice-shadows)
        analyze-timed (fn [x]
                        (try
                          ;(infer-frame x)
                          (u/with-timeout 3000 (let []
                                                 (println "   >>> Started analysing:" x)
                                                 (infer-frame x)))
                          (catch TimeoutException e (println "   !!! Analysis of" x "timed out!"))
                          (catch Exception e (println "   !!!" e))))
        get-assignable (fn [x]
                         (if (not= x nil)
                           (second (get-clauses-from-diagram x))))
        analysed-pairs (for [x pairs]
                         [(first x) 
                          (get-assignable (analyze-timed (first x))) 
                          (second x) 
                          (get-assignable (analyze-timed (second x)))])
        ;        compared-pairs (for [x analysed-pairs]
        ;                         (let [adv-clause (get-clauses-from-diagram (nth x 1))
        ;                               shadow-clause (get-clauses-from-diagram (nth x 3))
        ;                               comparison (ekeko-ajfx.analysis/compare-assignable-clauses 
        ;                                            (second shadow-clause)
        ;                                            (first adv-clause))]
        ;                           [(nth x 0) (nth x 2) comparison]))
        ]
    analysed-pairs))

(comment
  (write-advice-shadows)
  (inspect (read-advice-shadows))
  
  (let [] (inspect (do-complete-analysis)) nil)
  (time (let [] (do-complete-analysis) nil))
  (time (let [] (read-advice-shadows) nil))
  
  ;;;
  
  (u/with-timeout 3000 (time (get-clauses-from-diagram 
                               (do-analysis (first (nth (read-advice-shadows) 0))))))
  (inspect (second (nth (read-advice-shadows) 6)))
  
  
  (inspect (get-all-advice))
  (inspect (get-method-from-shadow 
             (nth (for [x (get-all-advice-and-shadows)] (second x)) 0)))
  (inspect (get-all-advice-and-shadows))
  (inspect (get-all-advice-bodies))
  (count (get-all-advice-bodies))
  (inspect (get-all-bodies))
  

  
  (inspect (filter
             (fn [x]
               (-> (first x) .getName (.startsWith "ajc$")))
             (get-all-bodies)))
  
  (inspect (for [x (get-all-advice)]
            (-> (first x) .getSourceLocation)))
  
  (let [q (ekeko [?a] (soot|method-name ?a "thrust"))
          method (first (nth (into [] q) 0))]
      (do-analysis method))
  
  (let [q (ekeko [?a] (soot|method-name ?a "processQueue"))
        method (first (nth (into [] q) 0))
        units (-> method .getActiveBody)
        ]
    (inspect units)
    ;(-> units (.follows (first units) (second units)))
    )
  
  (inspect (clojure.stacktrace/root-cause *e)))


;
; Get the units of a particular method
;;(inspect
;  (damp.ekeko/ekeko
;    [?unit]
;    (jsoot/soot-unit :JIdentityStmt ?unit)))

; Find the calls in a particular method
;(inspect 
;  (ekeko [?b]
;    (l/fresh [?a ?callee ?recv]
;             (jsoot/soot|method-soot|unit ?a ?b)
;             (soot|method-name ?a "helperMethod")
;             (method-methodCalls ?a ?callee ?b ?recv))))
;;;;;;;;;;;;;;;
;(ekeko-ajfx.diagram/reset-obj-id)
;(-> ekeko-ajfx.analysis/started-analysis .clear)
;(let [q (ekeko [?a] (soot|method-name ?a "getSize"))
;      method (first (first q))
;      frame (infer-frame method)]
;  frame)
;;;;;;;;;;;;;;
;(inspect
;  (let [q (ekeko [?a] (soot|method-name ?a "helper"))
;        method (first (first q))]
;    (new LoopNestTree (-> method .getActiveBody))))
;(let [q (ekeko [?a] (soot|method-name ?a "helper"))
;        method (first (first q))
;        body (-> method .getActiveBody)]
;    (showBlockCFG body))
