(ns 
  ^{:doc "Use AJFX to infer the frame conditions of Java/AspectJ code."
    :author "Tim Molderez" }
ekeko-ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
    [ekeko-ajfx.util :as dbg]
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

;(inspect (get-method-from-shadow
;           (second (nth (into [] (get-all-advice-and-shadows)) 55))))

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
        analysed-pairs (for [x pairs]
                         [(first x) (infer-frame (first x)) 
                          (second x) (infer-frame (second x))])
        compared-pairs (for [x analysed-pairs]
                         (let [adv-clause (get-clauses-from-diagram (nth x 1))
                               shadow-clause (get-clauses-from-diagram (nth x 3))
                               comparison (ekeko-ajfx.analysis/compare-assignable-clauses 
                                            (second shadow-clause)
                                            (first adv-clause))]
                           [(nth x 0) (nth x 2) comparison]))]
    compared-pairs))

(comment
  (write-advice-shadows)
  (clojure.core/type (read-advice-shadows))
  
  (dbg/with-timeout 500 (time (do-analysis (second (nth (read-advice-shadows) 11)))))
  (do-complete-analysis)
  
  (inspect (new java.io.File "."))
  (inspect (get-all-advice))
  (inspect (get-method-from-shadow 
             (nth (for [x (get-all-advice-and-shadows)] (second x)) 0)))
  (inspect (get-all-advice-and-shadows))
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
    (let [q (ekeko [?a] (soot|method-name ?a "ajc$perObjectBind"))
          method (first (first q))]
      method))
  
  (clojure.stacktrace/root-cause *e))


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
