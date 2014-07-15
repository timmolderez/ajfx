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
          }
          "
    :author "Tim Molderez" }
  ekeko-ajfx.diagram
  (:use [inspector-jay.core]))

"Ensures all objects within a graph have a unique identifier"
(def last-obj-id (atom 0))

(defn new-obj-id []
  "Obtain a unique identifier for a new object. 
   (Returned as a Clojure keyword, so it can be used as a key in maps..)"
  (keyword (str (swap! last-obj-id inc))))

(defn new-diagram [formals]
  "Create a new alias diagram, given the formal parameter names of the body to be analysed"
  (let [objs (zipmap 
               (for [x formals] (new-obj-id))
               (for [x formals] #{x (str "@" x)}))
        objs-this (assoc objs (new-obj-id) #{"this"})]
    {:obj objs-this
    :must-mod {}
    :may-mod {}
    :may-read {}}))

(defn add-object [diagram names]
  "Add a new object to a diagram. This object can be referred to by a given set of names."
  (assoc diagram :obj
    (assoc (diagram :obj)
      (new-obj-id) names)))

(defn add-name [diagram object])

(defn replace-name [diagram old-name new-name]
  )

(defn remove-name [diagram name])

(defn find-objs-by-name [diagram ])

(defn find-edges [diagram source-name field-name kinds])

(defn remove-edge [diagram source-name edge-name kinds])

(defn remove-object [diagram id])