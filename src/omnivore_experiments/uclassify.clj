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

(defn topic-classify [article]
  (Thread/sleep *uclassify-api-wait-ms*)
  (try+
   (let [response (http/get "http://uclassify.com/browse/uClassify/Topics/ClassifyText"
                            {:query-params {:readKey uclassify-api-key
                                            :text (.substring (:text article) 0 (min (.length (:text article)) 1900))
                                            :output "json"
                                            :version "1.01"}})
         parsed-response (json/parse-string (:body @response) true)]
     parsed-response)
   (catch Object _ (throw+))))

(defn get-predicted-topic [topic-classifier-res]
  (when-let [classes (:cls1 topic-classifier-res)]
    (first (apply max-key second classes))))
