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
    [ekeko-ajfx.util :as dbg])
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
  (let [cls-name (-> rhs .getType .toString)
        lhs-name (-> lhs .toString)
        obj-name (str "@" cls-name (d/new-obj-id))]
    (-> (d/remove-name diagram lhs-name)
      (d/add-object [obj-name lhs-name]))))

(defn array-write-stmt [diagram lhs rhs]
  (let [arr (-> lhs .getBaseBox .toString)]
    (d/add-edges diagram arr "element" :may-mod d/ANY-OBJ)))

(defn assign-stmt [diagram unit]
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
      :else (println "Unrecognized type of assignment!"))))

(defn if-stmt [diagram unit units]
  (let [begin-else (-> unit .getTarget)
        end-if (-> units (.getPredOf begin-else))
        has-else (instance? JGotoStmt end-if)
        end-else (if has-else (-> end-if .getTarget) nil)
        if-diagram (infer-frame-helper diagram units
                         (-> units (.getSuccOf unit))
                         (if has-else
                           end-if
                           (-> units (.getSuccOf end-if))))
        else-diagram (if has-else
                       (infer-frame-helper diagram units begin-else end-else)
                       diagram)
        merged-diagram (d/merge-diagrams if-diagram else-diagram)
        next-unit (if has-else end-else begin-else)]
    [merged-diagram next-unit]))

(defn try-catch-stmt [diagram unit]
  [diagram (-> unit .getTarget)]) ; Don't change the diagram, and just skip the catch blocks..

(defn loop-stmt [diagram unit units]
  (let [end-loop-body (-> unit .getTarget)
        find-next-unit (fn [unit]
                         (if (instance? JIfStmt unit)
                           (-> units (.getSuccOf unit))
                           (recur (-> units (.getSuccOf unit))))) 
        merged-diagram (d/multi-apply 
                         diagram
                         (fn [diagram]
                           (infer-frame-helper diagram units 
                             (-> units (.getSuccOf unit)) 
                             end-loop-body))
                         [[] []])]
    [merged-diagram (find-next-unit end-loop-body)]))

(defn cur-value [diagram object field]
  (let [must-found (filter
                     (fn [x] (= field (second x)))
                     ((diagram :must-mod ) object))]
    (if (empty? must-found)
      (filter
        (fn [x] (= field (second x)))
        (clojure.set/union ((diagram :may-mod) object) ((diagram :may-read) object)))
      must-found)))

(defn return-stmt [diagram unit]
  (let [value (-> unit .getOp)]
    (if (instance? JimpleLocal value)
      (d/add-return-val diagram (-> value .toString))
      diagram)))

;;; Interprocedural analysis ;;;

(defn compute-mappings [call-diag ctxt-diag formals actuals]
  (let [mapped-roots (d/multi-apply 
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
                            
                            new-pair (d/multi-apply pair
                                       (fn [pair call-tgt label]
                                         (let [ctxt-edges (reduce clojure.set/union
                                                            (for [x ctxt-objs] (cur-value ctxt-diag x label)))]
                                           (d/multi-apply pair
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
  (d/multi-apply 
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
            
            adjusted-edges (d/multi-apply
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
                                       new-musts (clojure.set/union old-musts-removed (merged-musts field))
                                       new-mays (clojure.set/union old-mays-removed (merged-mays field))]
                                   [new-musts new-mays])
                                 (let [new-mays (clojure.set/union (second edge-pair) (merged-mays field) (merged-musts field))]
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
  ; For each key in m
  (d/multi-apply {}
    (fn [inv-m key]
      ; For each value of key
      (d/multi-apply inv-m
        (fn [inv-m val] 
          (assoc inv-m val (conj (inv-m val) key)))
        (for [x (m key)] [x])))
    (for [x (keys m)] [x])))

(defn call-stmt [ctxt-diagram unit]
  (let [method (-> unit .getInvokeExpr .getMethod)
        return-name (if (instance? JAssignStmt unit)
                      (-> unit .getLeftOp .getName)
                      nil)
        call-diagram (infer-frame method)
        actuals (concat 
                  (if (not (instance? JStaticInvokeExpr (-> unit .getInvokeExpr)))
                    [(-> unit .getInvokeExpr .getBase)]) 
                  (-> unit .getInvokeExpr .getArgs))
        ;tmp (dbg method) 
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
  (let [;dbg (dbg/d unit)
        next (cond 
               (instance? IdentityStmt unit) (identity-stmt diagram unit)
               (and (instance? JAssignStmt unit) (not (-> unit .containsInvokeExpr))) (assign-stmt diagram unit)
               (-> unit .containsInvokeExpr) (call-stmt diagram unit)
               (and (instance? JGotoStmt unit) (not (instance? IdentityStmt (-> units (.getSuccOf unit))))) (loop-stmt diagram unit units)
               (instance? JGotoStmt unit) (try-catch-stmt diagram unit) 
               (instance? JIfStmt unit) (if-stmt diagram unit units)
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

(def infer-frame-from-scratch 
  (memo (fn [^SootMethod method]
          (println "Analysing method:" method)
          (let [body (-> method .getActiveBody)
                units (-> body .getUnits)
                diagram (-> (d/new-diagram [])
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
  (memo-clear! infer-frame-from-scratch))

(defn infer-frame [^SootMethod method]
  "Infer the aliasing diagram of a given method body
           (from which you can determine the body's frame condition)"
  (if (or 
        (not (-> method .hasActiveBody))
        (= (-> method .getName) "ajc$perObjectBind")) ; No idea why, but this particular method makes the memo cache throw an NPE..
    (let []
      ;(println "!! No body for method " method)
      (l/get-frame-from-library method))
    (infer-frame-from-scratch method)
    ))



(defn get-clauses-from-diagram [diagram]
  (let [roots (for [x (filter
                        (fn [x] (and 
                                  (-> (name x) (.startsWith  "@"))
                                  (contains? 
                                    (diagram :formals)
                                    (subs (name x) 1))))
                        (keys (diagram :names)))] [x])
        
        root-map (d/multi-apply
                   {}
                   (fn [m x]
                     (assoc m 
                       (first (d/find-objs-by-name diagram x))
                       (subs (name x) 1)))
                   (for [x roots] [x]))
        
        get-assignable (fn [read-edges read-paths clause]
                         (if (empty? read-edges)
                           [read-paths clause]
                           (let [cur-obj (first (clojure.set/intersection 
                                                  (set (keys read-edges))
                                                  (set (keys read-paths))))
                                 cur-path (read-paths cur-obj)
                                 new-read-paths (d/multi-apply
                                                  read-paths
                                                  (fn [r edge]
                                                    (assoc r (first edge) (str cur-path "." (second edge))))
                                                  (read-edges cur-obj))
                                 new-clause (d/multi-apply
                                              clause
                                              (fn [c edge]
                                                (conj c (str (read-paths cur-obj) "." (second edge))))
                                              (clojure.set/union 
                                                ((diagram :may-mod) cur-obj)
                                                ((diagram :must-mod) cur-obj)
                                                ))]
                             (recur 
                               (dissoc read-edges cur-obj)
                               new-read-paths
                               new-clause))))]
    (get-assignable (diagram :may-read) root-map [])))

(defn compare-assignable-clauses [master slave]
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