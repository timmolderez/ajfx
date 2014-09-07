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

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

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
    :must-mod {}
    :may-mod {}
    :may-read {}
    :return {}}))

(defn add-name [diagram objects name]
  "A set of objects can now be referred to by an additional name."
  (let [name-key (keyword name)
        name-set ((diagram :names) name-key)
        new-name-set (clojure.set/union name-set objects)
        new-names (assoc (diagram :names) name-key new-name-set)]
    (assoc diagram :names new-names)))


(defn add-object [diagram names]
  "Add a new object to a diagram (and generate an id). This object can be referred to by a number of names."
  (add-object-with-id diagram (new-obj-id) names))

(defn add-object-with-id [diagram id names]
  "Add a new object to a diagram with a given id. This object can be referred to by a number of names."
  (let [helper (fn [diagram names]
                 (if (empty? names)
                   diagram
                   (recur
                     (add-name diagram #{id} (first names))
                     (rest names))))]
    (helper diagram names)))

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
        new-ids (for [x source-ids] (new-obj-id)) 
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
      new-ids target-name)))

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