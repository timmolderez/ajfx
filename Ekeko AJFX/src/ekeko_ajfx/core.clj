(ns 
  ^{:doc "Main functions of AJFX, which you can use to infer the frame conditions of Java/AspectJ code."
    :author "Tim Molderez" }
ekeko-ajfx.core
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
    [ekeko-ajfx.util :as u]
    [ekeko-ajfx.soot :as s]
    [ekeko-ajfx.analysis :as a]
    [damp.ekeko.aspectj
     [weaverworld :as w]
     [soot :as ajsoot]]
    [damp.ekeko.soot
     [soot :as jsoot]])
  (:use [inspector-jay.core]
    [clojure.repl]
    [damp.ekeko logic]
    [damp.ekeko]
    )
  (:import [java.util.concurrent TimeoutException]))

(defn analyse-body [x]
  "Analyse a SootMethod to obtain its final aliasing diagram (, which is then used to determine the SootMethod's frame condition)"
  ;  (let []
  ;    (println "   >>> Started analysing:" x)
  ;    (try 
  ;      (a/infer-frame x)
  ;      (catch Throwable e e)))
  (try
    (u/with-timeout 3000 (let []
                           (println "   >>> Started analysing:" x)
                           (a/infer-frame x)))
    (catch TimeoutException e (println "   !!! Analysis of" x "timed out!"))
    (catch Exception e (let []
                         (println "   !!! Analysis of" x "threw an exception!" e)
                         e))))

(defn prepare-analysis []
  "Cleans up any state produced by a previous analysis, 
   most importantly the cache that stores the aliasing diagram of each analysed SootMethod" 
  (ekeko-ajfx.diagram/reset-obj-id)
  (-> a/started-analysis .clear)
  (a/clear-cache))

(defn get-assignable [diagram]
  "Obtain the assignable clause from a given aliasing diagram
   This clause describes which locations might be modified by the SootMethod corresponding to this diagram."
  (if (and (not= diagram nil) (not (instance? Throwable diagram)))
    (second (a/get-clauses-from-diagram diagram))
    diagram))

(defn analyse-all-advice-shadow-pairs []
  "Infer the assignable clauses of each advice and its shadows
   (!! There currently is no reliable way to relate a shadow to its SootMethod, so it might skip a few shadows..)" 
  (prepare-analysis)
  (for [x (s/read-advice-shadows)]
    [(first x) 
     (get-assignable (analyse-body (first x))) 
     (second x) 
     (get-assignable (analyse-body (second x)))]))

(defn analyse-all-advice [] 
  "Infer the assignable clauses of all advice in Ekeko+Soot-enabled projects"
  (prepare-analysis)
  (for [x (s/get-all-advice-bodies)]
    [(first x)
     (get-assignable (analyse-body (first x)))]))

(defn analyse-all-bodies []
  "Infer the assignable clauses of all SootMethods in Ekeko+Soot-enabled projects"
  (prepare-analysis)
  (for [x (s/get-all-bodies)]
    [(first x)
     (get-assignable (analyse-body (first x)))]))

; A few examples using the above functions..
(comment
  ; Open an inspector window with the results of the analysis (without dumping the results in the REPL too)
  (time (let [] (inspect (analyse-all-advice)) nil))
  (time (let [] (inspect (analyse-all-bodies)) nil))
  
  ; Analyse just one body..
  (u/with-timeout 3000 (let [] 
                         (prepare-analysis)
                         (analyse-body (first (nth (into [] (s/get-all-bodies)) 32 )))))
   
  ; Open an inspector with all advice bodies
  (let [] (inspect (s/get-all-advice-bodies)) nil)
  
  (count (s/get-all-bodies))
  
  (inspect (filter
             (fn [x] (-> (first x) .getName (.startsWith "ajc$")))
             (s/get-all-bodies)))
  
  (inspect (for [x (s/get-all-advice)]
            (-> (first x) .getSourceLocation)))
  
  (inspect (let [q (ekeko [?a] (s/soot|method-name ?a "helper3"))
                 method (first (nth (into [] q) 0))]
             (prepare-analysis)
             (get-assignable (analyse-body method))))
)