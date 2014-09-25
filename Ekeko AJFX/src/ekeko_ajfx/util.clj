(ns
  ^{:doc "Various utility functions"
    :author "Tim Molderez" }
  ekeko-ajfx.util
  (:refer-clojure :exclude [== type declare class])
  (:require [clojure.core.logic :as l]
            [clojure.stacktrace :as st]
            [damp.ekeko.aspectj
             [weaverworld :as w]
             [soot :as ajsoot]]
            [damp.ekeko.soot
             [soot :as jsoot]])
  (:use
    [inspector-jay.core]
    [clojure.repl]
    [damp.ekeko logic]
    [damp.ekeko]
    [damp.ekeko visualization]
    [damp.ekeko.visualization.view]
    [clojure.inspector :exclude [inspect]])
  (:import 
    [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal]
    [soot.jimple ThisRef ParameterRef]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph LoopNestTree]
    [org.aspectj.lang Signature]
    [java.lang Integer]
    [java.util.concurrent TimeoutException TimeUnit FutureTask] 
    [org.eclipse.jdt.core IJavaElement ITypeHierarchy IType IPackageFragment IClassFile ICompilationUnit
     IJavaProject WorkingCopyOwner IMethod]
    [org.eclipse.jdt.core.dom Expression IVariableBinding ASTParser AST IBinding Type TypeDeclaration 
     QualifiedName SimpleName ITypeBinding MethodDeclaration
     MethodInvocation ClassInstanceCreation SuperConstructorInvocation SuperMethodInvocation
     SuperFieldAccess FieldAccess ConstructorInvocation ASTNode ASTNode$NodeList CompilationUnit]
    [org.aspectj.weaver.patterns Pointcut AndPointcut]))

;
;(:import (java.util.concurrent TimeoutException TimeUnit FutureTask)
;           (clojure.lang LispReader$ReaderException))

(def VERBOSE true)

; Debugging macro, any function can be wrapped in (d ) 
(defmacro d[x] (if VERBOSE
                 `(let [x# ~x] (println "dbg:" '~x "=" x#) x#)
                 x)) 



(defn showUnitCFG
  "Use Ekeko's visualizer to show the unit control-flow graph of a Soot Body"
  [body]
  (let 
    [labelProvider (damp.ekeko.gui.EkekoLabelProvider.)
     graph (new ExceptionalUnitGraph body)
     nodes (-> body .getUnits)
     toString (fn [unit] (str (-> unit (.getTag "LineNumberTag")) ": " (.toString unit)))]
    (ekeko-visualize
      ; nodes
      (into []
        (map vector (map (fn [node] (toString node)) nodes))) 
      
      ; edges
      (into [] (mapcat identity (map (fn [node] ; for each unit
                                       (map (fn [succ] ; for each successor of unit
                                              [(toString node) (toString succ)])
                                         (-> graph (.getSuccsOf node))))
                                  nodes)))
      
      :node|label
      (fn [node] (.getText labelProvider node))
      :node|image 
      (fn [node] (.getImage labelProvider node))
      :edge|style 
      (fn [src dest] edge|directed)
      :layout
      layout|tree)))

(defn showBlockCFG
  "Use Ekeko's visualizer to show the block control-flow graph of a Soot Body"
  [body]
  (let 
    [labelProvider (damp.ekeko.gui.EkekoLabelProvider.)
     graph (new ExceptionalBlockGraph body)
     blocks (-> graph .getBlocks)
     toString (fn [block] (.toString block))]
    (ekeko-visualize
      ; nodes
     (into []
            (map vector (map (fn [block] (toString block)) blocks))) 
    
    ; edges
    (into [] (mapcat identity (map (fn [block] ; for each block
                                     (map (fn [succ] ; for each successor of block
                                            [(toString block) (toString succ)])
                                          (-> graph (.getSuccsOf block))))
                                   blocks)))
    
    :node|label
    (fn [node] (.getText labelProvider node))
    :node|image 
    (fn [node] (.getImage labelProvider node))
    :edge|style 
    (fn [src dest] edge|directed)
    :layout
    layout|tree)))

(defn getAdviceN
  "Get the nth advice body in the system (as a Soot method)" 
  [n]
  (first (nth 
           (ekeko [?method]
                  (l/fresh [?advice]
                           (w/advice ?advice)
                           (ajsoot/advice-soot|method ?advice ?method)))
           n)))

; Taken from https://github.com/flatland/clojail/blob/master/src/clojail/core.clj
(def ^{:doc "Create a map of pretty keywords to ugly TimeUnits"}
  uglify-time-unit
  (into {} (for [[enum aliases] {TimeUnit/NANOSECONDS [:ns :nanoseconds]
                                 TimeUnit/MICROSECONDS [:us :microseconds]
                                 TimeUnit/MILLISECONDS [:ms :milliseconds]
                                 TimeUnit/SECONDS [:s :sec :seconds]}
                 alias aliases]
             {alias enum})))


; Taken from https://github.com/flatland/clojail/blob/master/src/clojail/core.clj
(defmacro with-timeout [time & body]
  `(thunk-timeout (fn [] ~@body) ~time))

; Taken from https://github.com/flatland/clojail/blob/master/src/clojail/core.clj
(defn thunk-timeout
  "Takes a function and an amount of time to wait for thse function to finish
   executing. The sandbox can do this for you. unit is any of :ns, :us, :ms,
   or :s which correspond to TimeUnit/NANOSECONDS, MICROSECONDS, MILLISECONDS,
   and SECONDS respectively."
  ([thunk ms]
    (thunk-timeout thunk ms :ms nil)) ; Default to milliseconds, because that's pretty common.
  ([thunk time unit]
    (thunk-timeout thunk time unit nil))
  ([thunk time unit tg]
    (let [task (FutureTask. thunk)
          thr (if tg (Thread. tg task) (Thread. task))]
      (try
        (.start thr)
        (.get task time (or (uglify-time-unit unit) unit))
        (catch TimeoutException e
          (.cancel task true)
          (.stop thr) 
          (throw (TimeoutException. "Execution timed out.")))
        (catch Exception e
          (.cancel task true)
          (.stop thr) 
          (throw e))
        (finally (when tg (.stop tg)))))))

;(defmacro time-limited [ms & body]
;  `(let [f# (future ~@body)]
;     (.get f# ~ms java.util.concurrent.TimeUnit/MILLISECONDS)))

;(showBlockCFG (-> (getAdviceN 0) .getActiveBody))
;(inspect (-> (getAdviceN 0) .getActiveBody))
;
;(inspect (new LoopNestTree (-> (getAdviceN 0) .getActiveBody )))

;
;(inspect (ekeko [?method]
;                  (l/fresh [?advice]
;                           (w/advice ?advice)
;                           (ajsoot/advice-soot|method ?advice ?method))))
;
;(inspect (-> (getAdviceN 7) .getActiveBody))