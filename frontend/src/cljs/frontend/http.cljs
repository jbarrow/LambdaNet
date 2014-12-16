(ns frontend.http
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(def root "http://localhost:4000")

(defn json-parse
  "Returns ClojureScript data for the given JSON string."
  [line]
  (js->clj (JSON/parse line)))

(defn input-to-vector [data]
  (get (json-parse (str "{\"data\":" data "}")) "data"))

(defn create [init-type layers app-state]
  (go (let [response (<! (http/post (str root "/create")
                                    {:json-params {:init init-type
                                                   :layers layers}}))]
        (let [body (json-parse (:body response))]
          (if (not= body []) (do (swap! app-state assoc :initialized true)
                                 ;; (swap! app-state assoc :network body)
                                 ))))))
(defn train [data network app-state]
  (go (let [response (<! (http/post (str root "/train")
                                    {:json-params {:trainingdata (input-to-vector data)
                                                   :nw network}}))]
        (let [body (json-parse (:body response))]
          (if (not= body []) (do (swap! app-state assoc :trained true)
                                 (swap! app-state assoc :network body)))))))

(defn eval [inputs network app-state]
  (go (let [response (<! (http/post (str root "/eval")
                                    {:json-params {:inputs (input-to-vector inputs)
                                                   :network network}}))]
        (let [body (json-parse (:body response))]
          (if (not= body []) (swap! app-state assoc :result body))))))
