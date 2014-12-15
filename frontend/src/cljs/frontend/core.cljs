(ns frontend.core
    (:require [reagent.core :as reagent :refer [atom]]
              [frontend.viz :as viz]
              [goog.events :as events]
              [frontend.http :as http])
    (:import goog.History))

;; -------------------------
;; State

(defonce app-state (atom {:layers [] :init-type "normal" :layer-text ""}))

(defn get-state [k & [default]]
  (clojure.core/get @app-state k default))

(defn put! [k v]
  (swap! app-state assoc k v))

(defn put-in! [ks v]
  (swap! app-state assoc-in ks v)) 

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

(def init-types
  [{:key :normal :label "Normal"}
   {:key :uniform :label "Uniform"}
   {:key :t :label "Tea"}])

(def connectivity-types
  [{:key :full :label "Fully Connected"}])

(defn rand-id []
    (let [chars (apply vector "abcdefghijklmnopqrstuvwxyz0123456789")
                   num-chars (count chars)]
           (apply str
                         (take 8 (repeatedly #(get chars (rand-int num-chars)))))))

(defn create-layer []
  (let [new-layer {:connectivity-type :fully-connected
                   :neuron-count 3
                   :neuron-type :sigmoid
                   :id (rand-id)}]
    (put! :layers (conj (get-state :layers) new-layer))))

(defn remove-layer [layer]
  (put! :layers (filter #(not= (:id %) (:id layer)) (get-state :layers))))

(defn export [] (do (http/create (:init-type @app-state) (:layers @app-state))
                    (put! :layer-text
                          (str "Initialize using "
                               (:init-type @app-state)
                               " distribution, with the following layers: "
                               (:layers @app-state)))))

;; -------------------------
;; View

(defn atom-input [v ks type]
  [:input {:type (if type type "text")
           :value (if ks (get-in @app-state ks) @v)
           :on-change #(let [text (-> % .-target .-value)]
                         (if ks (put-in! ks text) 
                             (reset! v text)))}])

(defn selection-list [k items]
  [:select.form-control {:field :list :id :many-options
                         :on-change #(put-in! k (keyword (-> % .-target .-value)))} 
   (for [item items]
     [:option {:value (:key item)}
      (:label item)])])

(defn neuron-config [layer i] 
  [:div
   [selection-list [:layers i :neuron-type] neuron-types]
   [selection-list [:layers i :connectivity-type] connectivity-types]
   [atom-input layer [:layers i :neuron-count] "number"]
   ;; [:div (layer :id)]
   [:button {:on-click #(remove-layer layer)}
    "remove"]])

(defn main-page []
  (let [training-data (atom "[]")
        test-data (atom "[]")
        network [[[ 1.0, 1.0, 1.0]  [1.0, 1.0, 1.0]], [[1.0, 1.0, 1.0]]]] 
    (fn []
      [:div
       [:p "Training Data" [atom-input training-data]]
       [:button {:on-click create-layer}
        "Create new layer, fool."]
       [selection-list [:init-type] init-types]
       [:button {:on-click export}
        "Export"]
       (let [indexed (map-indexed vector (get-state :layers))]
         [:p "Layers:" (for [[i layer] indexed]
                         (neuron-config layer i))])
       [:code (:layer-text @app-state)]
       [viz/draw {} network]])))
;; -------------------------
;; Initialize app

(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))
