(ns frontend.core
    (:require [reagent.core :as reagent :refer [atom]]
              [frontend.viz :as viz]
              [goog.events :as events]
              [frontend.http :as http])
    (:import goog.History))

;; -------------------------
;; State

(defonce app-state (atom {:layers [] :network [[[ 0.5, 0.0, 1.0]  [0.5, 1.0, 0.0]], [[1.0, 1.0, 1.0]]] :init-type "normal" :layer-text ""}))

(defn get-state [k & [default]]
  (clojure.core/get @app-state k default))

(defn put! [k v]
  (swap! app-state assoc k v))

(defn put-in! [ks v]
  (swap! app-state assoc-in ks v)) 

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
  (let [new-layer {:connectivity :fully-connected
                   :ncount 3
                   :ntype :sigmoid
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

(defn train [data] (http/train data (get-state :network) app-state))

(defn evaluate [inputs] (http/eval inputs (get-state :network) app-state))

;; -------------------------
;; View

(defn atom-textarea [v ks type]
  [:textarea {:rows 4
              :columns 80
              :value (if ks (get-in @app-state ks) @v)
              :on-change #(let [text (-> % .-target .-value)]
                            (if ks (put-in! ks text) 
                                (reset! v text)))}])

(defn atom-input [v ks type]
  [:input {:type (if type type "text")
           :value (if ks (get-in @app-state ks) @v)
           :on-change #(let [text (-> % .-target .-value)]
                         (if ks (put-in! ks text) 
                             (reset! v text)))}])

(defn selection-list [k items]
  [:select {:field :list :id :many-options
                         :on-change #(put-in! k (keyword (-> % .-target .-value)))} 
   (for [item items]
     [:option {:value (:key item)}
      (:label item)])])

(defn neuron-config [layer i] 
  [:div
   [selection-list [:layers i :ntype] neuron-types]
   [selection-list [:layers i :connectivity] connectivity-types]
   [atom-input layer [:layers i :ncount] "number"]
   ;; [:div (layer :id)]
   [:button {:on-click #(remove-layer layer)}
    "remove"]])

(defn main-page []
  (let [training-data (atom (str "[[[[1.0, 0.0]], [[1.0]]],"
                                 "[[[1.0, 1.0]], [[0.0]]],"
                                 "[[[0.0, 1.0]], [[1.0]]],"
                                 "[[[0.0, 0.0]], [[0.0]]]]"))
        inputs (atom "[1, 1]")] 
    (fn []
      [:div.container
       [:div.row
        [:div.col-md-4
         [:button {:on-click create-layer}
          "Create layer."]]
        [:div.col-md-4
         [:button {:on-click export}
          "Initialize Network"]]
        [:div.col-md-4
         [selection-list [:init-type] init-types]]]
       (let [indexed (map-indexed vector (get-state :layers))]
         [:p {:class (if (< 0 (count (get-state :layers))) "visible" "hidden")}
          "Layers:" (for [[i layer] indexed]
                      (neuron-config layer i))])
       [:p "Training Data" [:br] [atom-textarea training-data]]
       [:button {:on-click (partial train @training-data)}
        "Train Network"]
       [:p "Inputs" [:br] [atom-textarea inputs]]
       [:button {:on-click (partial evaluate @inputs)}
        "Evaluate Inputs"]
       [:code (:layer-text @app-state) (str (:network @app-state))]
       [viz/draw {} (get-state :network)]])))
;; -------------------------
;; Initialize app

(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))
