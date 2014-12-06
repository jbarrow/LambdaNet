(ns frontend.interface
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.events :as events])
    (:import goog.History))

(defn atom-input [value]
    [:input {:type "text"
                        :value @value
                        :on-change #(reset! value (-> % .-target .-value))}])

