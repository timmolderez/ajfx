(ns
  ^{:doc "Defines the frame inference algorithm"
    :author "Tim Molderez" }
  ekeko-ajfx.analysis
  (:use 
    [inspector-jay.core])
  (:require 
    [ekeko-ajfx.diagram :as d])
  (:import 
    [soot.jimple IdentityStmt]
    [soot.jimple.internal JimpleLocal JInvokeStmt JIfStmt JGotoStmt JAssignStmt JInstanceFieldRef JNewExpr JTableSwitchStmt]
    [soot.jimple ThisRef ParameterRef ReturnStmt]
    [soot.toolkits.graph ExceptionalUnitGraph BriefBlockGraph ExceptionalBlockGraph LoopNestTree]
    [org.aspectj.lang Signature]
    [java.lang Integer]
    [org.eclipse.jdt.core IJavaElement ITypeHierarchy IType IPackageFragment IClassFile ICompilationUnit
     IJavaProject WorkingCopyOwner IMethod]
    [org.eclipse.jdt.core.dom Expression IVariableBinding ASTParser AST IBinding Type TypeDeclaration 
     QualifiedName SimpleName ITypeBinding MethodDeclaration
     MethodInvocation ClassInstanceCreation SuperConstructorInvocation SuperMethodInvocation
     SuperFieldAccess FieldAccess ConstructorInvocation ASTNode ASTNode$NodeList CompilationUnit]
    [org.aspectj.weaver.patterns Pointcut AndPointcut]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(d/reset-obj-id)

; Test case
(let [diagram (-> (d/new-diagram ["a" "b" "c"])
                (d/add-object #{"bla"})
                (d/add-name #{:1 :2} "d") 
                (d/add-edges "a" "f" :may-mod "b")
                (d/add-edges "d" "g" :may-mod "c")
                (d/add-edges-to-new-object "d" "h" :must-mod "z")
                (d/remove-edges :1 "g" :may-mod))]
  (d/find-edges diagram "a" "f" :may-mod)
  (d/reset-obj-id))

; This global set contains all methods for which the analysis was initiated, but not yet finished (in order to detect recursive calls)
(def started-analysis (new java.util.HashSet))
(-> started-analysis .clear)

(defn infer-frame [method]
  (let [body (-> method .getActiveBody)
        units (-> body .getUnits)
        diagram (-> (d/new-diagram [])
                  (d/add-object ["@@constant"]))]
    (println "Analysing method:" method)
    (if (-> started-analysis (.contains method))
      (let [] 
        (println "recursive call!" method)
        diagram)
      (let []
        (-> started-analysis (.add method))
        (let [frame (infer-frame-helper diagram units (-> units .getFirst) nil)]
          (-> started-analysis (.remove method))
          frame)))))

;;; Intraprocedural analysis ;;;

(defn infer-frame-helper [diagram units unit end-unit]
  (let [dbg (dbg unit)
        next (cond 
               (instance? IdentityStmt unit) (identity-stmt diagram unit)
               (and (instance? JAssignStmt unit) (not (-> unit .containsInvokeExpr))) (assign-stmt diagram unit)
               (-> unit .containsInvokeExpr) (call-stmt diagram unit)
               (instance? JGotoStmt unit) (loop-stmt diagram unit units) 
               (instance? JIfStmt unit) (if-stmt diagram unit units)
               (instance? JTableSwitchStmt unit) diagram ; TODO
               (instance? ReturnStmt unit) (return-stmt diagram unit) 
               :else diagram)
        next-unit (if (or (instance? JGotoStmt unit) (instance? JIfStmt unit))
                    (second next)
                    (-> units (.getSuccOf unit)))
        next-diagram (if (or (instance? JGotoStmt unit) (instance? JIfStmt unit))
                       (first next)
                       next)]
    (if (not (or (= next-unit nil) (-> next-unit (.equals end-unit))))
      (infer-frame-helper next-diagram units next-unit end-unit)
      next-diagram)))

(defn identity-stmt [diagram unit]
  (let [name (-> unit .getLeftOp .getName)]
    (-> (d/add-object diagram [name (str "@" name)])
      (d/add-formal name))))

(defn assign-stmt [diagram unit]
  (let [lhs (-> unit .getLeftOp)
        rhs (-> unit .getRightOp)]
    (cond
      ; Assignment type: a = new c ();
      (instance? JNewExpr rhs) (new-stmt diagram lhs rhs) 
      ; Assignment type: a = b;
      (and (instance? JimpleLocal lhs) (not (instance? JInstanceFieldRef rhs ))) (copy-stmt diagram lhs rhs)
      ; Assignment type: a.f = b;
      (instance? JInstanceFieldRef lhs) (field-write-stmt diagram lhs rhs) 
      ; Assignment type: a = b.f;
      (instance? JInstanceFieldRef rhs) (field-read-stmt diagram lhs rhs) 
      :else (println "Unrecognized type of assignment!"))))

(defn copy-stmt [diagram lhs rhs]
  "Processes an a = b; assignment"
  (if (instance? JimpleLocal rhs)
    (let [lhs-name (-> lhs .toString)
          rhs-name (-> rhs .toString)
          rhs-occurrences (d/find-objs-by-name diagram rhs-name)]
      (d/add-name (d/remove-name diagram lhs-name) rhs-occurrences lhs-name))
    (d/remove-name diagram (-> lhs .toString))))

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
                   "@@constant")
        lhs-found (d/find-objs-by-name diagram lhs-recv)
        after-rm (if (= 1 (count lhs-found))
                   (-> (d/remove-edges diagram (first lhs-found) lhs-field :may-mod)
                     (d/remove-edges (first lhs-found) lhs-field :must-mod)))]
    (if (or 
          (= 1 (count lhs-found))
          (not (empty? (d/find-edges after-rm lhs-recv lhs-field :must-mod))))
      (d/add-edges after-rm lhs-recv lhs-field :must-mod rhs-name)
      (d/add-edges after-rm lhs-recv lhs-field :may-mod rhs-name))))

(def last-new-id (atom 0))
(defn reset-new-id []
  (swap! last-new-id (fn [x] 0))) 
(defn new-id []
  (keyword (str (swap! last-new-id inc))))

(defn new-stmt [diagram lhs rhs]
  (let [cls-name (-> rhs .getType .toString)
        lhs-name (-> lhs .toString)
        obj-name (str "@" cls-name (new-id))]
    (d/add-object (d/remove-name diagram lhs-name) [obj-name])))

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

(defn loop-stmt [diagram unit units]
  (let [end-loop (-> unit .getTarget)
        merged-diagram (d/multi-apply 
                         diagram
                         (fn [diagram]
                           (infer-frame-helper diagram units 
                             (-> units (.getSuccOf unit)) 
                             end-loop))
                         [[] [] [] []])]
    [merged-diagram (-> units (.getSuccOf end-loop))]))

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
  (let [
        mapped-roots (d/multi-apply 
                       {}
                       (fn [m root] 
                         (let [index (.indexOf formals (subs (name root) 1))]
                           (dbg index) 
                           (if (not= index -1)
                             (assoc m 
                               (first ((call-diag :names) root)) 
                               ((ctxt-diag :names) (keyword (-> (nth actuals index) .toString)))) ; TODO, can we accidentally map to something in case of a constant actual?
                             (assoc m (first ((call-diag :names) root)) #{(d/new-obj-id)}))))
                       (for [x (filter
                                 (fn [x] (-> (name x) (.startsWith  "@")))
                                 (keys (call-diag :names)))] [x]))
        
;        map-ctxt-edges (fn [pair call-tgt label]
;                         (let [m (first pair)
;                               ctxt-reads (second pair)
;                               ctxt-objs (m call-obj)
;                               
;                               ctxt-edges (reduce clojure.set/union
;                                            (for [x ctxt-objs] (cur-value ctxt-diag x label)))]
;                           (assoc m call-tgt
;                             (set (for [x ctxt-edges]
;                                    (first x))))))
        
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
                                                    (assoc ctxt-reads ctxt-obj [new-read-tgt label])]
                                                   [(assoc m call-tgt (clojure.set/union 
                                                                      cur-val
                                                                      (for [x new-val] (first x))))
                                                    ctxt-reads]
                                                   )))
                                             (for [x ctxt-objs] [x]))
                                           
                                           

;                                           (assoc m call-tgt
;                                             (set (for [x ctxt-edges]
;                                                    (first x))))
                                           ))
                                       read-edges)
                            
                            new-m (d/multi-apply m
                                    (fn [m call-tgt label]
                                      (let [ctxt-edges (reduce clojure.set/union
                                                         (for [x ctxt-objs] (cur-value ctxt-diag x label)))]
                                        (assoc m call-tgt
                                          (set (for [x ctxt-edges]
                                                 (first x))))))
                                    read-edges)]
                        (if (empty? new-objects) 
                  [new-m ctxt-reads] 
                  (recur [new-m ctxt-reads] new-objects))))]
    (dbg call-diag)
    (dbg ctxt-diag) 
    (dbg (map-objects (dbg mapped-roots) (keys mapped-roots)))))

(defn adjust-edges [ctxt-diag call-diag call2ctxt ctxt2call]
  (d/multi-apply 
    ctxt-diag
    (fn [ctxt-diag ctxt-obj]
      (let [call-objs (ctxt2call ctxt-obj)
            
            ; is there another object in ctxt that might map to the same object in call? (if so, we should convert must-be-modified to may-be-modified)
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
            
            ; determine the fields that *must* be modified in ctxt, iff the field in *all* corresponding objs of call are must-be-modified  
            must-mod-fields (reduce
                              (fn [x y] (clojure.set/intersection (keys x) (keys y)))
                              must-field-groups)
            
            merged-musts (merge-with map-and-union must-field-groups)
            
            may-field-groups (for [x call-objs]
                               [x (group-by
                                    (fn [edge] (second edge))
                                    ((call-diag :may-mod) x))])
            merged-mays (merge-with map-and-union may-field-groups)
            
            adjusted-edges (d/multi-apply
                             [((ctxt-diag :must-mod) ctxt-obj) ((ctxt-diag :may-mod) ctxt-obj)]
                             (fn [edge-pair field]
                               (if (and 
                                     is-uniq-ctxt-obj
                                     (contains? must-mod-fields field)
                                     (not (empty? (merged-musts field))))
                                 (let [old-musts-removed (remove
                                                           (fn [x] (= (second x) field))
                                                           (edge-pair first))
                                       old-mays-removed (remove
                                                           (fn [x] (= (second x) field))
                                                           (edge-pair second))
                                       new-musts (clojure.set/union old-musts-removed (merged-musts field))
                                       new-mays (clojure.set/union old-mays-removed (merged-mays field))]
                                   [new-musts new-mays])
                                 (let [new-mays (clojure.set/union (edge-pair second) (merged-mays field) (merged-musts field))]
                                   [(edge-pair first) new-mays])))
                             (for [x (clojure.set/union 
                                       (set (keys merged-musts))
                                       (set (keys merged-mays)))] [x]))
            
            new-musts (assoc (ctxt-diag :must-mod) ctxt-obj (adjusted-edges first))
            new-mays (assoc (ctxt-diag :may-mod) ctxt-obj (adjusted-edges second))]
        (-> ctxt-diag
          (assoc :must-mod new-musts)
          (assoc :may-mod new-mays))))
    (for [x (keys ctxt2call)] [x])))

(defn invert-mapping [m]
  (d/multi-apply {}
    (fn [inv-m key]
      (d/multi-apply inv-m
        (fn [inv-m val] (assoc inv-m val (conj (inv-m val) key)))
        (for [x (m key)] [x])))
    (for [x (keys m)] [x])))

(defn call-stmt [ctxt-diagram unit]
  (let [method (-> unit .getInvokeExpr .getMethod)
        return-name (if (instance? JAssignStmt unit)
                      (-> unit .getLeftOp .getName)
                      nil)
        call-diagram (infer-frame method)
        actuals (concat [(-> unit .getInvokeExpr .getBase) ; receiver object, which you can also consider an (implicit) actual argument
                         (-> unit .getInvokeExpr .getArgs)]) ; actual arguments
        call2ctxt (compute-mappings call-diagram ctxt-diagram (call-diagram :formals) actuals)
        ctxt2call (dbg (invert-mapping call2ctxt))
        embedded-diagram (adjust-edges ctxt-diagram call-diagram call2ctxt ctxt2call)]
    (if (= return-name nil)
      embedded-diagram
      (-> embedded-diagram
        (d/remove-name return-name)
        (d/add-name 
          (reduce clojure.set/union
            (for [x (call-diagram :return)] (call2ctxt x))) 
          return-name))))) 