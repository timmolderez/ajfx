(ns
  ^{:doc "A library of aliasing diagrams (for methods where we don't have access to the body)"
    :author "Tim Molderez" }
  ekeko-ajfx.library
  (:require 
    [ekeko-ajfx.diagram :as d]))

(def frame-library {:Test
                    {:helper3 (d/new-diagram [])}})