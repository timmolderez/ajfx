(ns
  ^{:doc "Defines the frame inference algorithm"
    :author "Tim Molderez" }
  ekeko-ajfx.analysis
  (:use 
    [inspector-jay.core]
    [clojure.core.memoize])
  (:require 
    [ekeko-ajfx.diagram :as d]
    [ekeko-ajfx.library :as l]
    [ekeko-ajfx.util :as u])
  (:import
    [java.util HashSet]
    [soot SootMethod Unit PatchingChain PrimType] 
    [soot.jimple IdentityStmt Stmt ThisRef ParameterRef ReturnStmt]
    [soot.jimple.internal JimpleLocal JInvokeStmt JStaticInvokeExpr JIfStmt JGotoStmt JAssignStmt 
     JInstanceFieldRef JNewExpr JTableSwitchStmt JIdentityStmt JNewArrayExpr JArrayRef]))

(declare infer-frame)
(declare infer-frame-helper)
(declare infer-frame-from-scratch)

;;; Intraprocedural analysis ;;; 

(defn identity-stmt [diagram ^JIdentityStmt unit]
  "Analyse an identify statement (which defines formal parameters)" 
  (let [name (-> unit .getLeftOp .getName)
        is-primitive (instance? PrimType (-> unit .getRightOp .getType))
        new-id (if is-primitive
                 (first (d/find-objs-by-name diagram d/ANY-OBJ))
                 (d/new-obj-id))]
    (-> (d/add-object-with-id diagram new-id [name (str "@" name)])
      (d/add-formal name))))

(defn copy-stmt [diagram lhs rhs]
  "Processes an a = b; assignment"
  (let [lhs-name (-> lhs .toString)
        rhs-name (-> rhs .toString)
        rhs-is-var (instance? JimpleLocal rhs) 
        rhs-occurrences (if rhs-is-var
                          (d/find-objs-by-name diagram rhs-name)
                          (d/find-objs-by-name diagram d/ANY-OBJ))]
    (d/add-name (d/remove-name diagram lhs-name) rhs-occurrences lhs-name)))

(defn field-read-stmt [diagram lhs rhs]
  "Processes an a = b.f; assignment" 
  (let [lhs-name (-> lhs .toString)
        rhs-recv (-> rhs .getBase .toString)
        rhs-field (-> rhs .getField .getName)
        after-rm (d/remove-name diagram lhs-name)
        found-rhs (d/find-objs-by-name diagram rhs-recv) 
        found-read (d/find-edges after-rm rhs-recv rhs-field :may-read)
        found-may (d/find-edges after-rm rhs-recv rhs-field :may-mod)
        found-must (d/find-edges after-rm rhs-recv rhs-field :must-mod)]
    (cond
      (empty? found-read) (d/add-edges-to-new-object after-rm rhs-recv rhs-field :may-read lhs-name)
      (empty? found-must) (let [tgts (clojure.set/union 
                                       (set (for [x found-read] (second x))) 
                                       (set (for [x found-may] (second x))))]
                            (d/add-name after-rm tgts lhs-name))
      :else (d/add-name after-rm (set (for [x found-must] (second x))) lhs-name))))

(defn field-write-stmt [diagram lhs rhs]
  "Processes an a.f = b; assignment" 
  (let [lhs-recv (-> lhs .getBase .toString)
        lhs-field (-> lhs .getField .getName)
        rhs-name (if (instance? JimpleLocal rhs)
                   (-> rhs .toString)
                   d/ANY-OBJ)
        lhs-found (d/find-objs-by-name diagram lhs-recv)
        after-rm (if (= 1 (count lhs-found))
                   (-> (d/remove-edges diagram (first lhs-found) lhs-field :may-mod)
                     (d/remove-edges (first lhs-found) lhs-field :must-mod)))]
    (if (or 
          (= 1 (count lhs-found))
          (not (empty? (d/find-edges after-rm lhs-recv lhs-field :must-mod))))
      (d/add-edges after-rm lhs-recv lhs-field :must-mod rhs-name)
      (d/add-edges after-rm lhs-recv lhs-field :may-mod rhs-name))))

(defn new-stmt [diagram lhs rhs]
  "Analyse a = new c();" 
  (let [cls-name (-> rhs .getType .toString)
        lhs-name (-> lhs .toString)
        obj-name (str "@" cls-name (d/new-obj-id))]
    (-> (d/remove-name diagram lhs-name)
      (d/add-object [obj-name lhs-name]))))

(defn array-write-stmt [diagram lhs rhs]
  "Analyse a[x] = b;" 
  (let [arr (-> lhs .getBaseBox .toString)]
    (d/add-edges diagram arr "element" :may-mod d/ANY-OBJ)))

(defn assign-stmt [diagram unit]
  "Analyse an assignment statement" 
  (let [lhs (-> unit .getLeftOp)
        rhs (-> unit .getRightOp)]
    (cond
      ; Assignment type: a = new c ();
      (or (instance? JNewExpr rhs) (instance? JNewArrayExpr rhs)) (new-stmt diagram lhs rhs) 
      ; Assignment type: a = b;
      (and (instance? JimpleLocal lhs) (not (instance? JInstanceFieldRef rhs ))) (copy-stmt diagram lhs rhs)
      ; Assignment type: a.f = b;
      (instance? JInstanceFieldRef lhs) (field-write-stmt diagram lhs rhs) 
      ; Assignment type: a = b.f;
      (instance? JInstanceFieldRef rhs) (field-read-stmt diagram lhs rhs)
      ; Assignment type: a[x] = b;
      (instance? JArrayRef lhs) (array-write-stmt diagram lhs rhs)
      :else (let [] (println "Unrecognized type of assignment:" unit "::" (diagram :tag))
              diagram)
      )))

(defn if-stmt [diagram unit units end-unit]
  "Analyse an if statement" 
  (if (-> units (.follows unit (-> unit .getTarget))) ; This shouldn't happen..
    [diagram (-> units (.getSuccOf unit))]
    (let [begin-else (-> unit .getTarget)
        end-if (-> units (.getPredOf begin-else))
        has-else (instance? JGotoStmt end-if)
        end-else (if has-else (-> end-if .getTarget) nil)
        if-diagram (infer-frame-helper diagram units
                         (-> units (.getSuccOf unit))
                         (if has-else
                           end-if
                           (-> units (.getSuccOf end-if))))
        else-diagram (if (and has-else (-> units (.follows (-> end-if .getTarget) end-if)))
                       (infer-frame-helper diagram units begin-else end-else)
                       diagram)
        merged-diagram (d/merge-diagrams if-diagram else-diagram)
        next-unit (if (and has-else (-> units (.follows (-> end-if .getTarget) end-if))) 
                     end-else
                     begin-else)
         bounded-next-unit (if (and ; If next-unit goes outside the body being analysed, the analysis should stop here.
                                 (not= nil end-unit) 
                                 (-> units (.follows next-unit end-unit))) 
                      nil
                      next-unit)]
    [merged-diagram bounded-next-unit])))

(defn try-catch-stmt [diagram unit]
  "Ignore catch blocks in try-catch statements (for now..)" 
  [diagram (-> unit .getTarget)])

(defn loop-stmt [diagram loop-unit units]
  "Analyse any kind of loop statement" 
  (let [end-loop-body (-> loop-unit .getTarget)
        find-next-unit (fn [unit]
                         (cond 
                           (= unit nil) [nil false]
                           (instance? JIfStmt unit) (if (= (-> unit .getTarget)
                                                               (-> units (.getSuccOf loop-unit)))
                                                      [(-> units (.getSuccOf unit)) true]
                                                      [nil false])
                           (instance? JGotoStmt unit) [nil false]
                           :else (recur (-> units (.getSuccOf unit)))))
        next (find-next-unit end-loop-body)
        merged-diagram (if (second next)
                         (u/multi-apply 
                           diagram
                           (fn [diagram]
                             (infer-frame-helper diagram units 
                               (-> units (.getSuccOf loop-unit)) 
                               end-loop-body))
                           [[] []])
                         diagram) 
        ]
    [merged-diagram (first next)]))

(defn cur-value [diagram object field]
  "Retrieve the current potential values of a particular field of object" 
  (let [must-found (filter
                     (fn [x] (= field (second x)))
                     ((diagram :must-mod ) object))]
    (if (empty? must-found)
      (filter
        (fn [x] (= field (second x)))
        (clojure.set/union ((diagram :may-mod) object) ((diagram :may-read) object)))
      must-found)))

(defn return-stmt [diagram unit]
  "Analyse a return statement" 
  (let [value (-> unit .getOp)]
    (if (instance? JimpleLocal value)
      [(d/add-return-val diagram (-> value .toString)) nil]
      [diagram nil])))

;;; Interprocedural analysis ;;;

(defn compute-mappings [call-diag ctxt-diag formals actuals]
  "Compute the mapping between objects in call-diag to the objects in ctxt-diag" 
  (let [mapped-roots (u/multi-apply 
                       {(first (d/find-objs-by-name call-diag d/ANY-OBJ)) (d/find-objs-by-name ctxt-diag d/ANY-OBJ)}
                       (fn [m root] 
                         (let [index (.indexOf formals (subs (name root) 1))] 
                           (if (not= index -1)
                             (let [actual (nth actuals index)
                                   ignorable (or 
                                               (= actual nil) 
                                               (and (not (instance? java.util.ArrayList actual)) (instance? PrimType (-> actual .getType))))]
                               (if ignorable
                                 m
                                 (assoc m 
                                   (first ((call-diag :names) root)) 
                                   ((ctxt-diag :names) (keyword (-> actual .toString))))))
                             (assoc m (first ((call-diag :names) root)) #{(d/new-obj-id)}))))
                       (for [x (filter
                                 (fn [x] (-> (name x) (.startsWith  "@")))
                                 (keys (call-diag :names)))] [x]))
        
        map-objects (fn [pair objects]
                      (let [m (first pair)
                            ctxt-reads (second pair)
                            
                            call-obj (first objects)
                            ctxt-objs (m call-obj)
                            read-edges ((call-diag :may-read) call-obj)
                            new-objects (concat
                                          (rest objects)
                                          (for [x read-edges] (first x)))
                            
                            new-pair (u/multi-apply pair
                                       (fn [pair call-tgt label]
                                         (let [ctxt-edges (reduce clojure.set/union
                                                            (for [x ctxt-objs] (cur-value ctxt-diag x label)))]
                                           (u/multi-apply pair
                                             (fn [pair ctxt-obj]
                                               (let [m (first pair)
                                                     ctxt-reads (second pair)
                                                     cur-val (m call-tgt)
                                                     new-val (cur-value ctxt-diag ctxt-obj label)
                                                     new-read-tgt (keyword (str (name ctxt-obj) label))]
                                                 ; Is there a corresponding object in ctxt? If not, create it by adding a new read edge.
                                                 (if (empty? new-val)
                                                   [(assoc m call-tgt (clojure.set/union cur-val #{new-read-tgt} 
                                                                        (for [x new-val] (first x))))
                                                    (assoc ctxt-reads ctxt-obj #{[new-read-tgt label]})]
                                                   [(assoc m call-tgt (clojure.set/union 
                                                                        cur-val
                                                                        (for [x new-val] (first x))))
                                                    ctxt-reads]
                                                   )))
                                             (for [x ctxt-objs] [x]))))
                                       read-edges)]
                        (if (empty? new-objects) 
                          new-pair
                          (recur new-pair new-objects))))] 
    (map-objects [mapped-roots (ctxt-diag :may-read)] (keys mapped-roots))))

(defn adjust-edges [ctxt-diag call-diag call2ctxt ctxt2call]
  "Update the field values in ctxt-diag to reflect the new values produced by call-diag" 
  (u/multi-apply 
    ctxt-diag
    (fn [ctxt-diag ctxt-obj]
      (let [call-objs (ctxt2call ctxt-obj)
            
            ; Is there another object in ctxt that might map to the same object in call? 
            ; (If so, we should convert must-be-modified edges to may-be-modified)
            is-uniq-ctxt-obj (every?
                               (fn [x] (= 1 (count (call2ctxt x))))
                               call-objs)
            
            map-edges (fn [edges]
                        (set (mapcat identity (for [x edges]
                                                (for [y (call2ctxt (first x))]
                                                  [y (second x)])))))
            
            map-and-union (fn [& edge-set]
                            (let [mapped (for [x edge-set] (map-edges x))]
                              (reduce clojure.set/union mapped)))   
            
            must-field-groups (for [x call-objs]
                                (group-by
                                  (fn [edge] (second edge))
                                  ((call-diag :must-mod) x)))
            
            ; Determine the fields that *must* be modified in ctxt, iff the field in *all* corresponding objs of call are must-be-modified  
            must-mod-fields (reduce
                              (fn [x y] (clojure.set/intersection (keys x) (keys y)))
                              must-field-groups)
            
            merged-musts (apply merge-with map-and-union must-field-groups)
            
            may-field-groups (for [x call-objs]
                               (group-by
                                 (fn [edge] (second edge))
                                 ((call-diag :may-mod) x)))
            
            merged-mays (apply merge-with map-and-union may-field-groups)
            
            adjusted-edges (u/multi-apply
                             [((ctxt-diag :must-mod) ctxt-obj) ((ctxt-diag :may-mod) ctxt-obj)]
                             (fn [edge-pair field] 
                               (if (and 
                                     is-uniq-ctxt-obj
                                     (contains? must-mod-fields field)
                                     (not (empty? (merged-musts field))))
                                 (let [old-musts-removed (remove
                                                           (fn [x] (= (second x) field))
                                                           (first edge-pair))
                                       old-mays-removed (remove
                                                          (fn [x] (= (second x) field))
                                                          (second edge-pair))
                                       new-musts (clojure.set/union (set old-musts-removed) (set (merged-musts field)))
                                       new-mays (clojure.set/union (set old-mays-removed) (set (merged-mays field)))]
                                   [new-musts new-mays])
                                 (let [new-mays (clojure.set/union (set (second edge-pair)) (set (merged-mays field)) (set (merged-musts field)))]
                                   [(first edge-pair) new-mays])))
                             (for [x (clojure.set/union 
                                       (set (keys merged-musts))
                                       (set (keys merged-mays)))] [x]))
            
            new-musts (assoc (ctxt-diag :must-mod) ctxt-obj (first adjusted-edges))
            new-mays (assoc (ctxt-diag :may-mod) ctxt-obj (second adjusted-edges))]
        (-> ctxt-diag
          (assoc :must-mod new-musts)
          (assoc :may-mod new-mays))))
    (for [x (keys ctxt2call)] [x])))

(defn invert-mapping [m]
  "Invert a mapping of objects in the body being called, to the objects in the calling context,
   such that you get a mapping from objects in the context, to the objects in the called body" 
  ; For each key in m
  (u/multi-apply {}
    (fn [inv-m key]
      ; For each value of key
      (u/multi-apply inv-m
        (fn [inv-m val] 
          (assoc inv-m val (conj (inv-m val) key)))
        (for [x (m key)] [x])))
    (for [x (keys m)] [x])))

(defn call-stmt [ctxt-diagram unit]
  "Analyse a method/constructor call" 
  (let [method (-> unit .getInvokeExpr .getMethod)
        return-name (if (instance? JAssignStmt unit)
                      (-> unit .getLeftOp .getName)
                      nil)
        call-diagram (infer-frame method)
        actuals (concat 
                  (if (not (instance? JStaticInvokeExpr (-> unit .getInvokeExpr)))
                    [(-> unit .getInvokeExpr .getBase)]) 
                  (-> unit .getInvokeExpr .getArgs))
        mapping-results (compute-mappings call-diagram ctxt-diagram (call-diagram :formals) actuals)
        call2ctxt (first mapping-results)
        new-ctxt-diagram (assoc ctxt-diagram :may-read (second mapping-results)) 
        ctxt2call (invert-mapping call2ctxt)
        embedded-diagram (adjust-edges new-ctxt-diagram call-diagram call2ctxt ctxt2call)]
    (if (= return-name nil)
      embedded-diagram
      (-> embedded-diagram
        (d/remove-name return-name)
        (d/add-name 
          (reduce clojure.set/union
            (for [x (call-diagram :return)] (call2ctxt x))) 
          return-name)))))

;;; Frame inference analysis ;;;

; This global set contains all methods for which the analysis was initiated, but not yet finished (in order to detect recursive calls)
(def ^HashSet started-analysis (new java.util.HashSet))
(-> started-analysis .clear)

(defn infer-frame-helper [diagram ^PatchingChain units ^Stmt unit ^Stmt end-unit]
  "Analyse the unit statement, which is part of units, and this body ends at end-unit" 
  (let [;dbg (println unit "::" (first (-> unit .getTags)) "::" (diagram :tag))
        next (cond 
               (instance? IdentityStmt unit) (identity-stmt diagram unit)
               (and (instance? JAssignStmt unit) (not (-> unit .containsInvokeExpr))) (assign-stmt diagram unit)
               (-> unit .containsInvokeExpr) (call-stmt diagram unit)
               (and (instance? JGotoStmt unit) (not (instance? IdentityStmt (-> units (.getSuccOf unit))))) (loop-stmt diagram unit units)
               (instance? JGotoStmt unit) (try-catch-stmt diagram unit) 
               (instance? JIfStmt unit) (if-stmt diagram unit units end-unit)
               (instance? JTableSwitchStmt unit) diagram ; TODO 
               (instance? ReturnStmt unit) (return-stmt diagram unit) 
               :else diagram) ; Any other kind of stmt is ignored.. (throws , enter/exit monitor, breakpoint)
        ^Stmt next-unit (if (sequential? next)
                          (second next)
                          (-> units (.getSuccOf unit)))
        next-diagram (if (sequential? next)
                       (first next)
                       next)
        ;tmp (println next-diagram)
        ]
    (if (not (or (= next-unit nil) (-> next-unit (.equals end-unit))))
      (infer-frame-helper next-diagram units next-unit end-unit)
      next-diagram)))

; Analyse a method body to produce its final aliasing diagram; this function is memoized to avoid analysing the same method twice
(def infer-frame-from-scratch 
  (memo (fn [^SootMethod method]
          ;(println "Analysing method:" method)
          (let [body (-> method .getActiveBody)
                units (-> body .getUnits)
                diagram (-> (d/new-diagram [])
                          (d/add-tag (-> method .getSignature)) 
                          (d/add-object [d/ANY-OBJ]))]
            (if (-> started-analysis (.contains method))
              (let [] 
                (println "recursive call!" method)
                diagram)
              (let []
                (-> started-analysis (.add method))
                (let [frame (infer-frame-helper diagram units (-> units .getFirst) nil)]
                  (-> started-analysis (.remove method))
                  frame)))))))

(defn clear-cache []
  "Clear the memoization cache used by the analysis" 
  (memo-clear! infer-frame-from-scratch))

(defn infer-frame [^SootMethod method]
  "Infer the aliasing diagram of a given method body
   (from which you can determine the body's assignable and accessible clauses.)"
  (if (or 
        (not (-> method .hasActiveBody))
        (= (-> method .getName) "ajc$perObjectBind")) ; No idea why, but only this particular method makes the memo cache throw an NPE..
    (l/get-frame-from-library method)
    (infer-frame-from-scratch method)
    ))



(defn get-clauses-from-diagram [diagram]
  "Given an aliasing diagram, return an accessible clause and an assignable clause,
   which respectively describe what may be accessed, and what may be modified by a method." 
  (let [roots (for [x (filter
                        (fn [x] (and 
                                  (-> (name x) (.startsWith  "@"))
                                  (contains? 
                                    (set (diagram :formals))
                                    (subs (name x) 1))
                                    ))
                        (keys (diagram :names)))] 
                  [x])
        
        root-map (u/multi-apply
                   [{} (diagram :may-read)]
                   (fn [pair x]
                       (let [root-obj (first (d/find-objs-by-name diagram x))
                             cur-val ((second pair) root-obj)]
                         [(assoc (first pair) root-obj 
                            (subs (name x) 1))
                        (assoc (second pair) root-obj (clojure.set/union #{} cur-val))]))
                   roots)
        
        get-assignable (fn [read-edges read-paths clause]
                           (let [worklist (clojure.set/intersection 
                                          (set (keys read-edges))
                                          (set (keys read-paths)))]
                             (if (empty? worklist)
                               [read-paths clause]
                               (let [cur-obj (first worklist)
                                   cur-path (read-paths cur-obj)
                                   new-read-paths (u/multi-apply
                                                    read-paths
                                                    (fn [r tgt label]
                                                      (assoc r tgt (str cur-path "." label)))
                                                    (read-edges cur-obj))
                                   new-clause (u/multi-apply
                                                clause
                                                (fn [c tgt label]
                                                  (conj c (str (read-paths cur-obj) "." label)))
                                                (clojure.set/union 
                                                  ((diagram :may-mod) cur-obj)
                                                  ((diagram :must-mod) cur-obj)
                                                  ))]
                               (recur 
                                 (dissoc read-edges cur-obj)
                                 new-read-paths
                                 new-clause)))))]
    (get-assignable (second root-map) (first root-map) [])))

(defn compare-assignable-clauses [master slave]
  "Compare the assignable clauses of two SootMethods, such that slave may not modify more than master
   A list of modifications that are not allowed is returned.
   " 
  (filter
    (fn [x]
      (some
        (fn [y] (-> x (.startsWith y)))
        master))
    slave)
  ) 

;(defn infer-frame-slow [^SootMethod method]
;  (if (-> method .hasActiveBody)
;    (infer-frame-from-scratch method)
;    (let []
;      (println "!! No body for method " method)
;      (l/get-frame-from-library method))))

;(let [diagram (-> (d/new-diagram ["a" "b" "c"])
;                (d/add-object #{"bla"})
;                (d/add-name #{:1 :2} "d") 
;                (d/add-edges "a" "f" :may-mod "b")
;                (d/add-edges "d" "g" :may-mod "c")
;                (d/add-edges-to-new-object "d" "h" :must-mod "z")
;                (d/remove-edges :1 "g" :may-mod))]
;  (d/find-edges diagram "a" "f" :may-mod)
;  (d/reset-obj-id))

;(def last-new-id (atom 0))
;(defn reset-new-id []
;  (swap! last-new-id (fn [x] 0))) 
;(defn new-id []
;  (keyword (str (swap! last-new-id inc))))