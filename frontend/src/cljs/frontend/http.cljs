(ns frontend.http
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(defn get-that []
  (go (let [response (<! (http/get "https://api.github.com/users" {:with-credentials? false}))]
        (prn (:status response))
        (prn (map :login (:body response))))))

(defn create [init-type layers]
  (go (let [response (<! (http/post "http://localhost:8000/create"
                                    {:json-params {:init init-type
                                                   :layers layers}}))])))
