(ns frontend.core
    (:require [reagent.core :as reagent :refer [atom]]
              [goog.events :as events])
    (:import goog.History))

;; -------------------------
;; State

(defonce app-state (atom {:text "Hello, this is: "}))

(defn get-state [k & [default]]
  (clojure.core/get @app-state k default))

(defn put! [k v]
  (swap! app-state assoc k v))

(defn json-parse
  "Returns ClojureScript data for the given JSON string."
  [line]
  (js->clj (JSON/parse line)))

;; -------------------------
;; Neural Logic

(def neuron-types
  [{:key :sigmoid :label "Sigmoid"}
   {:key :tangent :label "Hyberbolic Tangent"}
   {:key :linear :label "Rectified Linear"}])

(def connectivity-types
  [{:key :full :label "Fully Connected"}])

;; -------------------------
;; View

(defn atom-input [v k type]
  [:input {:type (if type type "text")
           :value (if k (k @v) @v)
           :on-change #(let [text (-> % .-target .-value)]
                         (if k (swap! v assoc k text) 
                             (reset! v text)))}])

(defn selection-list [v k items]
  [:select.form-control {:field :list :id :many-options
                         :on-change #(swap! v assoc k (-> % .-target .-value))}
   (for [item items]
     [:option {:key (:key item)}
      (:label item)])])

(defn neuron-config [layer] 
  [:div
   [selection-list layer :neuron-type neuron-types]
   [selection-list layer :connectivity-type connectivity-types]
   [atom-input layer :neuron-count "number"]])

(defn create-layer []
  (put! :layers (conj (get-state :layers)
                      (atom {:connectivity-type :fully-connected
                             :neuron-count 3
                             :neuron-type :sigmoid}))))

(defn main-page []
  (let [training-data (atom "[]")
        test-data (atom "[]")] 
    (fn []
      [:div
       [:p "Training Data" [atom-input training-data]]
       [:button {:on-click create-layer}
        "Create new layer."]
       [:p "Layers:" (for [layer (get-state :layers)]
                       (neuron-config layer))]])))
;; -------------------------
;; Initialize app

(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))
