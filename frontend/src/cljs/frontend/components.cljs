(ns frontend.components
  (:require [reagent.core :as reagent :refer [atom]]))

(def point-defaults
  {:stroke "black"
   :stroke-width 2
   :fill "grey"
   :r 5})

(def rect-defaults
  {:stroke "blue"
   :stroke-width 2
   :fill "grey"
   :height 20
   :width 40})

(def segment-defaults
  {:stroke "black"
   :stroke-width 2})

(defn point [x y]
  [:circle
   (merge point-defaults
          {:cx x
           :cy y})])

(defn segment [from to]
  [:line
   (merge segment-defaults
          {:x1 (x from) :y1 (y from)
           :x2 (x to) :y2 (y to)})])

(defn rect [x y]
  [:rect
   (merge rect-defaults
          {:x x
           :y y})]) 
