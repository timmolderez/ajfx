(ns
  ^{:doc "Defines the aliasing diagrams to be used by the frame inference algorithm, and its operations
          
          To give an idea of what the data structure behind an alias diagram looks like:
          {:objects { // Maps objects to the names of that object
             :0 #{'to' 'this'}
             :1 #{'amount'}
             :2 nil}
           :must-be-modified { // Maps the source of edges to its names and its targets
             :0 #{['f' :1] ['g' :3]}
             ...
             }
           :may-be-modified { ... }
           :may-be-read { ... }
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
                (concat id-sets id-sets))
        names-this (assoc names :this #{(new-obj-id)})]
    {:names names-this
    :must-mod {}
    :may-mod {}
    :may-read {}}))

(defn add-name [diagram object name]
  "An object can now be referred to by an additional name."
  (let [name-key (keyword name)
        name-set ((diagram :names) name-key)
        new-name-set (clojure.set/union name-set #{object})
        new-names (assoc (diagram :names) name-key new-name-set)]
    (assoc diagram :names new-names)))


(defn add-object [diagram name]
  "Add a new object to a diagram. This object is referred to by a certain name."
  (add-name diagram (new-obj-id) name))

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
                   (let [src (x first)
                         tgts (x second)]
                     (for [y tgts]
                       [src y])))]
    (mapcat identity filtered))) ; Flatten by one level

(defn add-edges [diagram source-name label kind target-name]
  "Add edges from all nodes with source-name"
  (let [source-ids (find-objs-by-name diagram source-name)
        target-ids (find-objs-by-name diagram target-name)
        all-edges (diagram kind)
        add-edge (fn [edges src tgt]
                   (let [new-set (conj (edges src) tgt)]
                     (assoc edges src new-set)))
        new-edges ()]
    (assoc diagram kind new-edges)
    ))

(defn add-edges-to-new-object [diagram source-name label kind])

(defn remove-edges [diagram source-name label kind])