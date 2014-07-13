(ns
  ^{:doc "Defines the aliasing diagrams to be used by the frame inference algorithm, and its operations
          
          To give an idea of what the data structure behind an alias diagram looks like:
          {:objects {
             :0 #{'to' 'this'}
             :1 #{'amount'}
             :2 nil}
           :must-be-modified {
             [0 1]
             [2 1]
             }
          }
          "
    :author "Tim Molderez" }
  ekeko-ajfx.diagram)

"Ensures all objects within a graph have a unique identifier"
(def last-obj-id (atom 0))

(defn get-new-obj-id []
  "Obtain a unique identifier for a new object"
  (swap! last-obj-id inc))

