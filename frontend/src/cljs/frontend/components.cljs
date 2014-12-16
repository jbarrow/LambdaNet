(ns frontend.components
  (:require [reagent.core :as reagent :refer [atom]]))

(def point-defaults
  {:stroke "black"
   :stroke-width 2
   :fill "white"
   :r 5})

(def rect-defaults
  {:stroke "blue"
   :stroke-width 2
   :fill "grey"
   :height 30
   :width 60})

(defn point [x y]
  [:circle
   (merge point-defaults
          {:cx x
           :cy y})])

(defn segment [from to thickness]
  (println thickness) 
  [:line
   (merge {:x1 (:x from) :y1 (:y from)
           :x2 (:x to) :y2 (:y to)}
          (or thickness {:stroke-width 1}))])

(defn rect [pos]
  [:rect
   (merge rect-defaults pos)]) 
