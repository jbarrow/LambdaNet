(ns frontend.viz
  (:require [reagent.core :as reagent :refer [atom]]
            [frontend.components :as c]
            [goog.events :as events]))

(defn neuron-position [x y]
  {:x (+ 40 (* x 100))
   :y (+ 40 (* y 50))})

(defn draw-layer-neurons [layer x]
  [:g.layer
   (for [[y weight] (map vector (iterate inc 0) (first layer))] 
     [:g.neuron
      [c/rect (neuron-position x y)]
      [neuron-connections layer x (neuron-position x y)]])])

(defn to-right-side [pos]
  (let [x (+ (:x pos) 40)
        y (+ (:y pos) 10)]
    {:x x :y y}))

(defn to-left-side [pos]
  (let [y (+ (:y pos) 10)]
    {:x (:x pos) :y y}))

(defn neuron-connections [layer x start-pos]
  [:g.connections
   (for [[y weight] (map vector (iterate inc 0) layer)]
     [c/segment (to-right-side start-pos)
      (to-left-side (neuron-position (inc x) y))])])

(defn draw-network [network]
  [:g.network
   (for [[x layer] (map vector (iterate inc 0) network)]
     [draw-layer-neurons layer x])
   [c/rect (neuron-position (count network) 0)]])

(defn draw [{:keys [width height]} network]
  [:svg
   {:width (or width 600)
    :height (or height 400)
    :style {:border "1px solid #ddd"}}
   [:text {:style {:-webkit-user-select "none"
                   :-moz-user-select "none"
                   :user-select "none"}
           :x 20 :y 20 :font-size 20}
    "Neural Network"]
   [draw-network network]])
