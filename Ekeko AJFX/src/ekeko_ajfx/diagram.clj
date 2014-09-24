(ns
  ^{:doc "Defines the aliasing diagrams to be used by the frame inference algorithm, and its operations
          
          To give an idea of what the data structure behind an alias diagram looks like:
          {:names { // Maps objects to the names of that object
             :bla #{:1 :2}}
           :must-mod { // Maps the source of edges to its names and its targets
             :0 #{['f' :1] ['g' :3]}
             ...
             }
           :may-mod { ... }
           :may-read { ... }
           :return #{:1 :3}
          }"
    :author "Tim Molderez" }
  ekeko-ajfx.diagram
  (:use [inspector-jay.core]))


(def ANY-OBJ "%ANY")

"Ensures all objects within a graph have a unique identifier"
(def last-obj-id (atom 0))

(defn reset-obj-id []
  "Reset the last-object-identifier to 0"
  (swap! last-obj-id (fn [x] 0))) 

(defn new-obj-id []
  "Obtain a unique identifier for a new object. 
   (Returned as a Clojure keyword, so it can be used as a key in maps..)"
  (keyword (str (swap! last-obj-id inc))))

(defn new-diagram [formals]
  "Create a new alias diagram, given the formal parameter names of the body to be analysed"
  (let [root-names (for [x formals] (keyword (str "@" x)))
        names (for [x formals] (keyword x)) 
        id-sets (for [x formals] #{(new-obj-id)})
        names (zipmap
                (concat root-names names)
                (concat id-sets id-sets))]
    {:names names
     :formals [] 
    :must-mod {}
    :may-mod {}
    :may-read {}
    :return #{}}))

(defn add-name [diagram objects name]
  "A set of objects can now be referred to by an additional name."
  (let [name-key (keyword name)
        name-set ((diagram :names) name-key)
        new-name-set (clojure.set/union name-set objects)
        new-names (assoc (diagram :names) name-key new-name-set)]
    (assoc diagram :names new-names)))

(defn add-formal [diagram name]
  (let [new-formals (conj (diagram :formals) name)]
    (assoc diagram :formals new-formals))) 

(defn add-object-with-id [diagram id names]
  "Add a new object to a diagram with a given id. This object can be referred to by a number of names."
  (let [helper (fn [diagram names]
                 (if (empty? names)
                   diagram
                   (recur
                     (add-name diagram #{id} (first names))
                     (rest names))))]
    (helper diagram names)))


(defn add-object [diagram names]
  "Add a new object to a diagram (and generate an id). This object can be referred to by a number of names."
  (add-object-with-id diagram (new-obj-id) names))

(defn replace-name [diagram old-name new-name]
  "All objects that are referred to by old-name will now be referred to be new-name instead."
  (let [new-key (keyword new-name)
        old-key (keyword old-name)
        new-ids (clojure.set/union
                  ((diagram :names) new-key)
                  ((diagram :names) old-key))
        new-names (assoc (dissoc (diagram :names) old-key)
                    new-key new-ids)]
    (assoc diagram :names new-names)))

(defn remove-name [diagram name]
  "Remove a name from a diagram, without removing the objects it refers to"
  (let [new-names (dissoc (diagram :names) (keyword name))]
    (assoc diagram :names new-names)))

(defn find-objs-by-name [diagram name]
  "Find all objects that may be referred to be a certain name"
  ((diagram :names) (keyword name)))

(defn find-edges [diagram source-name label kind]
  "Retrieve the edges with a given name at the source, with a particular edge label, of a particular edge kind"
  (let [source-ids (find-objs-by-name diagram source-name)
        all-edges (diagram kind)
        matching-edges (select-keys all-edges source-ids)
        filtered (for [x matching-edges]
                   (let [src (first x)
                         tgts (second x)] 
                     (for [y tgts]
                       [src (first y) (second y)])))] 
    (mapcat identity filtered))) ; Flatten by one level

(defn add-edges [diagram source-name label kind target-name]
  "Add edges from all nodes with source-name to all nodes with target-name"
  (let [source-ids (find-objs-by-name diagram source-name)
        target-ids (find-objs-by-name diagram target-name)
        new-tgts (set (for [x target-ids]
                        [x label]))
        add-edges-helper (fn [edges sources]
                           (if (empty? sources)
                             edges
                             (recur
                               (let [cur-source (first sources)
                                     cur-tgts (edges cur-source)]
                                 (assoc edges cur-source (clojure.set/union cur-tgts new-tgts)))
                               (rest sources))))]
    (assoc diagram kind (add-edges-helper (diagram kind) source-ids))))

(defn add-edges-to-new-object [diagram source-name label kind target-name]
  "For each node with source-name, create a new object and add an edge to it. This new object is named target-name"
  (let [source-ids (find-objs-by-name diagram source-name)
        new-ids (for [x source-ids]
                  (keyword (str (name x) label))) 
        add-edges-helper (fn [edges sources ids]
                           (if (empty? sources)
                             edges
                             (recur
                               (let [cur-source (first sources) 
                                     cur-tgts (edges cur-source)]
                                 (assoc edges cur-source (clojure.set/union cur-tgts #{[(first ids) label]})))
                               (rest sources)
                               (rest ids))))]
    ; Note that we don't need to call add-object since the new objects are nameless..
    (add-name
      (assoc diagram kind (add-edges-helper (diagram kind) source-ids new-ids))
      (set new-ids) target-name)))

(defn remove-edges [diagram source-id label kind]
  "Remove the edges starting at a given object, with a given label"
  (let [edges ((diagram kind) source-id)
        filtered-edges (set (filter
                              (fn [x] (not= label (second x)))
                              edges))
        new-edges (assoc (diagram kind) source-id filtered-edges)]
    (assoc diagram kind new-edges)))

(defn add-return-val [diagram name]
  (let [return-val (diagram :return)
        objs (find-objs-by-name diagram name)
        new-return-val (clojure.set/union return-val objs)]
    (assoc diagram :return new-return-val)))

(defn multi-apply [x func args]
  (let [helper (fn [input args]
                 (if (empty? args)
                   input
                   (recur (apply func input (first args)) (rest args))))]
    (helper x args)))

(defn union-edges [one two]
  (multi-apply
    one
    (fn [map key]
      (let [old-val (map key)
            new-val (two key)]
        (assoc map key (clojure.set/union old-val new-val))))
    (for [x (keys two)] [x])))

(defn intersect-edges [one two]
  (multi-apply
    [{} {}]
    (fn [pair key]
      (let [one-val (one key)
            two-val (two key)
            one-labels (set (for [x one-val] (second x)))
            two-labels (set (for [x two-val] (second x)))
            intersect-labels (clojure.set/intersection one-labels two-labels)
            union-val (clojure.set/union one-val two-val)
            filtered (filter
                       (fn [x] (contains? intersect-labels (second x)))
                       union-val)
            compl (filter 
                    (complement (fn [x] (contains? intersect-labels (second x)))) 
                    union-val)]
        [(if (empty? filtered) 
           (first pair)
           (assoc (first pair) key filtered))
         (if (empty? compl)
           (second pair)
           (assoc (second pair) key compl))]))
    (for [x (clojure.set/union (set (keys one)) (set (keys two)))] [x])))

(defn merge-diagrams [d1 d2]
  (let [new-diag (new-diagram [])
        merged-names (multi-apply (d1 :names)
                       (fn [names-map key]
                         (let [old-val (names-map key)
                               new-val ((d2 :names) key)]
                           (assoc names-map key (clojure.set/union old-val new-val))))
                       (for [x (keys (d2 :names))] [x]))
        merged-reads (union-edges (d1 :may-read) (d2 :may-read))
        intersect (intersect-edges (d1 :must-mod) (d2 :must-mod))
        merged-musts (first intersect)
        merged-mays (union-edges 
                      (union-edges (d1 :may-mod) (d2 :may-mod)) 
                      (second intersect))]
    (-> (new-diagram []) 
      (assoc :names merged-names)
      (assoc :formals (d1 :formals)) 
      (assoc :may-read merged-reads)
      (assoc :may-mod merged-mays)
      (assoc :must-mod merged-musts)
      (assoc :return (clojure.set/union (d1 :return) (d2 :return))))))

;(intersect-edges
;  {:1 #{[:4 "f"] [:3 "g"] [:4 "g"] [:5 "h"]} :2 #{[:1 "f"] [:3 "g"]}} 
;  {:1 #{[:2 "f"] [:3 "g"]} :2 #{[:1 "f"] [:3 "g"]} :3 #{[:1 "f"] [:3 "g"]}})