(ns frontend.viz
  (:require [reagent.core :as reagent :refer [atom]]
            [frontend.components :as c]
            [goog.events :as events]))

(defn neuron-position [x y]
  {:x (+ 60 (* x 140))
   :y (+ 60 (* y 80))})

(defn draw-layer-neurons [layer x]
  [:g.layer
   (for [[y weight] (map vector (iterate inc 0) (first layer))] 
     [:g.neuron
      [neuron-connections layer x y]
      [c/rect (neuron-position x y)]])])

(defn to-right-side [pos]
  (let [x (+ (:x pos) 60)
        y (+ (:y pos) 15)]
    {:x x :y y}))

(defn to-left-side [pos]
  (assoc pos :y (+ 15 (:y pos))))

(defn thickness [weight]
  {:stroke-width (* weight 4)})

(defn neuron-connections [layer start-x start-y]
  [:g.connections
   (for [[y weights] (map vector (iterate inc 0) layer)]
     [c/segment (to-right-side (neuron-position start-x start-y))
      (to-left-side (neuron-position (inc start-x) y))
      (thickness (get weights y))])])

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
