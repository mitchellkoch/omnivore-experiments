(ns ^{:doc "Call the uClassify API"
      :author "Mitchell Koch"}
  omnivore-experiments.uclassify
  (:use plumbing.core
        [slingshot.slingshot :only [try+ throw+]])
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [org.httpkit.client :as http]))

(def uclassify-api-key (str/trim-newline (slurp "uclassify-api-key.txt")))

(def ^:dynamic *uclassify-api-wait-ms* 200)

(defn topic-classify [text]
  (Thread/sleep *uclassify-api-wait-ms*)
  (let [response (http/get "http://uclassify.com/browse/uClassify/Topics/ClassifyText"
                           {:query-params {:readKey uclassify-api-key
                                           :text (.substring text 0 (min (.length text) 1900))
                                           :output "json"
                                           :version "1.01"}})]
    (try+
     (json/parse-string (:body @response) true)
     (catch Object _ (throw+ @response)))))

(defn get-predicted-topic [topic-classifier-res]
  (->> topic-classifier-res
       :cls1
       (apply max-key second)
       first))
