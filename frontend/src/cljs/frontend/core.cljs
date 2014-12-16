(ns frontend.core
  (:require [reagent.core :as reagent :refer [atom]]
            [frontend.viz :as viz]
            [goog.events :as events]
            [frontend.http :as http])
  (:import goog.History))

;; -------------------------
;; State

(defonce app-state (atom {:layers []
                          :network [[[ 0.5, 0.0, 1.0]  [0.5, 1.0, 0.0]], [[1.0, 1.0, 1.0]]]
                          :init-type "normal"
                          :layer-text ""
                          :initialized false
                          :trained false}))

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

(defn export [] (do (http/create (:init-type @app-state) (:layers @app-state) app-state)
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
              :class "form-control"
              :value (if ks (get-in @app-state ks) @v)
              :on-change #(let [text (-> % .-target .-value)]
                            (if ks (put-in! ks text) 
                                (reset! v text)))}])

(defn atom-input [v ks type]
  [:input {:type (if type type "text")
           :value (if ks (get-in @app-state ks) @v)
           :class "form-control"
           :on-change #(let [text (-> % .-target .-value)]
                         (if ks (put-in! ks text) 
                             (reset! v text)))}])

(defn selection-list [k items opt-class]
  [:select {:field :list :id :many-options
            :class (str "form-control " opt-class )
            :on-change #(put-in! k (keyword (-> % .-target .-value)))} 
   (for [item items]
     [:option {:value (:key item)}
      (:label item)])])

(defn neuron-config [layer i] 
  [:div
   [selection-list [:layers i :ntype] neuron-types "inline no-left"]
   [selection-list [:layers i :connectivity] connectivity-types "inline"]
   [atom-input layer [:layers i :ncount] "number"]
   ;; [:div (layer :id)]
   [:button {:on-click #(remove-layer layer) :class "form-control btn-default"}
    "remove"]])

(defn main-page []
  (let [training-data (atom (str "[[[[1.0, 0.0]], [[1.0]]],"
                                 "[[[1.0, 1.0]], [[0.0]]],"
                                 "[[[0.0, 1.0]], [[1.0]]],"
                                 "[[[0.0, 0.0]], [[0.0]]]]"))
        inputs (atom "[1, 1]")] 
    (fn []
      [:div.container
       [:img {:src "logo.png"}]
       [:div.separator]
       [:div.section
        [:h2 "1. Set up network."]
        [:button {:on-click create-layer :class "form-control btn-default"}
         "Create layer"]
        [selection-list [:init-type] init-types]
        [:button {:on-click export :class "form-control btn-default"}
         "Initialize Network"]
        (let [indexed (map-indexed vector (get-state :layers))]
          [:ol {:class (if (< 0 (count (get-state :layers))) "visible" "hidden")}
           (for [[i layer] indexed]
             [:li (neuron-config layer i)])])]
       [:div.section {:class (if (get-state :initialized) "visible" "hidden")} 
        [:h2 "2. Train network."]
        [atom-textarea training-data]
        [:button {:on-click (partial train @training-data) :class "form-control btn-default"}
         "Train"]
        ]
       [:div.section {:class (if (get-state :trained) "visible" "hidden")} 
        [:h2 "3. Evaluate network."]
        [atom-textarea inputs]
        [:button {:on-click (partial evaluate @inputs)
                  :class "form-control btn-default"}
         "Evaluate"]
        [:pre {:class (if (get-state :result) "visible" "hidden")}
         (str (:result @app-state))]]
       [:div {:class (if (get-state :initialized) "visible" "hidden")} 
        [viz/draw {} (get-state :network)]]
       ])))
;; -------------------------
;; Initialize app

(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))
