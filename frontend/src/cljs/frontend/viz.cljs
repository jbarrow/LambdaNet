(ns frontend.viz
  (:require [reagent.core :as reagent :refer [atom]]
            [frontend.components :as c]
            [goog.events :as events]))

(defn draw-layer [layer]
  (let [count (first (first layer))
        elements (apply vector
                        (conj [:g] (map-indexed (fn [i weight]
                                                  [c/rect 200 (+ 20 (* i 50))])
                                                (first layer))))]
    elements))

(defn draw-network [network]
  (println network)
  [:g
   [c/rect 20 20]
   (for [layer network]
     [draw-layer layer])])

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
