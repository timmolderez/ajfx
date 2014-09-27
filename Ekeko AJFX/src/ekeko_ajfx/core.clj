(ns 
  ^{:doc "Main functions of AJFX, which you can use to infer the frame conditions of Java/AspectJ code."
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
    [ekeko-ajfx.analysis])
  (:import [java.util.concurrent TimeoutException]))

(defn get-all-advice []
  "Retrieve all advice of Ekeko-enabled projects (as BcelAdvice instances)"
  (set (ekeko [?adv] (w/advice ?adv))))

(defn get-all-advice-and-shadows []
  "Retrieve all advice and their corresponding join point shadows (as a list of BcelAdvice,ProgramElement pairs)" 
  (set (ekeko [?adv-soot ?meth]
         (l/fresh [?adv]
           (w/advice ?adv)
           (w/advice-shadow ?adv ?meth)
           (ajsoot/advice-soot|method ?adv ?adv-soot)))))

(defn get-all-advice-bodies []
  "Retrieve all advice bodies of Ekeko+Soot-enabled projects (as SootMethod instances)" 
  (set (ekeko [?method]
         (l/fresh [?advice]
           (w/advice ?advice)
           (ajsoot/advice-soot|method ?advice ?method)))))

(defn get-all-bodies []
  "Retrieve all method and advice bodies of Ekeko+Soot-enabled projects (as SootMethod instances)"
  (ekeko [?method]
         (l/fresh []
           (jsoot/soot :method ?method))))

(defn get-method-from-shadow [shadow]
  "Try to determine the SootMethod described in a join point shadow (as obtained via w/advice-shadow)
   (!! This doesn't work very reliably.. only the .toString value of a shadow seems to be of any use, but it often doesn't contain enough information..)"
  (let [str (-> shadow .toString)
        open (-> str (.indexOf "("))
        close (-> str (.lastIndexOf ")"))
        ?sig (subs str (inc open) close)
        query (ekeko [?m] (ekeko-ajfx.soot/soot|method-sig ?m ?sig))]
    (first (first query))))

(defn write-advice-shadows []
  "Write all advice - join point shadow pairs to a file.."
  (let [pairs (for [x (get-all-advice-and-shadows)]
                (try
                  [(-> (first x) .getSignature)
                   (-> (get-method-from-shadow (second x)) .getSignature)]
                  (catch Exception e (println "!!! No body found for shadow:" (second x)))))
        filtered (remove (fn [x] (= x nil))
                   pairs)]
    (spit "advice-shadow-pairs.txt" (pr-str (into [] filtered)))))

(defn read-advice-shadows []
  "Read the advice - join point shadow pairs produced by (write-advice-shadows)"
  (let [pairs (load-file "advice-shadow-pairs.txt")]
    (into [] (set (for [x pairs]
                  [(first (first (ekeko [?m] (ekeko-ajfx.soot/soot|advice-sig-full ?m (first x)))))
                   (first (first (ekeko [?m] (ekeko-ajfx.soot/soot|method-sig-full ?m (second x)))))])))))

(defn analyse-body [x]
  "Analyse a SootMethod to obtain its final aliasing diagram (, which is then used to determine the SootMethod's frame condition)" 
  (try
    (u/with-timeout 3000 (let []
                           (println "   >>> Started analysing:" x)
                           (infer-frame x)))
    (catch TimeoutException e (println "   !!! Analysis of" x "timed out!"))
    (catch Exception e (println "   !!! Analysis of" x "caused an exception:" e))))

(defn prepare-analysis []
  "Cleans up any state produced by a previous analysis, most importantly the cache that stores the aliasing diagram of each analysed SootMethod" 
  (ekeko-ajfx.diagram/reset-obj-id)
  (-> ekeko-ajfx.analysis/started-analysis .clear)
  (ekeko-ajfx.analysis/clear-cache))

(defn get-assignable [diagram]
  "Obtain the assignable clause from a given aliasing diagram
   This clause describes which locations might be modified by the SootMethod corresponding to this diagram."
  (if (not= diagram nil)
    (second (get-clauses-from-diagram diagram))))

(defn analyse-all-advice-shadow-pairs []
  "Infer the assignable clauses of each advice and its shadows
   (!! There currently is no reliable way to relate a shadow to its SootMethod, so it might skip a few shadows..)" 
  (prepare-analysis)
  (for [x (read-advice-shadows)]
    [(first x) 
     (get-assignable (analyse-body (first x))) 
     (second x) 
     (get-assignable (analyse-body (second x)))]))

(defn analyse-all-advice [] 
  "Infer the assignable clauses of all advice in Ekeko+Soot-enabled projects"
  (prepare-analysis)
  (for [x (get-all-advice-bodies)]
    [(first x)
     (get-assignable (analyse-body (first x)))]))

(defn analyse-all-bodies []
  "Infer the assignable clauses of all SootMethods in Ekeko+Soot-enabled projects"
  (prepare-analysis)
  (for [x (get-all-bodies)]
    [(first x)
     (get-assignable (analyse-body (first x)))]))

; A few examples using the above functions..
(comment
  ; Open an inspector window with the results of the analysis (without dumping the results in the REPL too)
  (time (let [] (inspect (analyse-all-advice)) nil))
  (time (let [] (inspect (analyse-all-bodies)) nil))
  
  ; Analyse just one body..
  (u/with-timeout 3000 (get-clauses-from-diagram 
                         (do-analysis (first (nth (into [] (get-all-advice-bodies)) 7 )))))
   
  ; Open an inspector with all advice bodies
  (let [] (inspect (get-all-advice-bodies)) nil)
  
  (count (get-all-bodies))
  
  (inspect (filter
             (fn [x] (-> (first x) .getName (.startsWith "ajc$")))
             (get-all-bodies)))
  
  (inspect (for [x (get-all-advice)]
            (-> (first x) .getSourceLocation)))
  
  (let [q (ekeko [?a] (soot|method-name ?a "toUnsignedString"))
          method (first (nth (into [] q) 0))]
      (do-analysis method))
  
  (let [q (ekeko [?a] (soot|method-name ?a "processQueue"))
        method (first (nth (into [] q) 0))
        units (-> method .getActiveBody)]
    (inspect units)))