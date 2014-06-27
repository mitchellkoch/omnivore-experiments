(ns ^{:doc "Freebase-related functions and caches"
      :author "Mitchell Koch"}
  omnivore-experiments.freebase
  (:use plumbing.core
        [slingshot.slingshot :only [throw+]])
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [org.httpkit.client :as http]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [alandipert.enduro :as e]))

(def freebase-api-key (str/trim-newline (slurp "freebase-api-key.txt")))

(def ^:dynamic *freebase-api-wait-ms* 200)

(defn mql-read [query]
  (Thread/sleep *freebase-api-wait-ms*)
  (let [response (http/get "https://www.googleapis.com/freebase/v1/mqlread"
                           {:query-params {:query (json/generate-string query)
                                           :key freebase-api-key}})]
    (json/parse-string (:body @response) true)))

(defn search [query]
  (Thread/sleep *freebase-api-wait-ms*)
  (let [response (http/get "https://www.googleapis.com/freebase/v1/search"
                           {:query-params {:query query
                                           :key freebase-api-key}})]
    (json/parse-string (:body @response) true)))

(defn topic [topic-id filter-param]
  (Thread/sleep *freebase-api-wait-ms*)
  (let [response (http/get (str "https://www.googleapis.com/freebase/v1/topic" topic-id)
                           {:query-params {:key freebase-api-key
                                           :filter filter-param}})]
    (json/parse-string (:body @response) true)))

(def cached-entity-mid->type-set (e/file-atom {} "freebase-caches/entity-mid-to-type-set.edn"))

(defn entity-mid->type-set [mid]
  (if-let [res (@cached-entity-mid->type-set mid)]
    res
    (do #_(println "Freebase API call: requesting types for" mid)
        (let [type-set (->> (mql-read [{:id mid, :type []}])
                            :result first :type set)]
          (e/swap! cached-entity-mid->type-set assoc mid type-set)
          type-set))))

(def cached-entity-mid->notable-type (e/file-atom {} "freebase-caches/entity-mid-to-notable-type.edn"))

(defn entity-mid->notable-type [mid]
  (let [notable-type (->> (topic mid "/common/topic/notable_types")
                          :property :/common/topic/notable_types 
                          :values first :id)]
    (e/swap! cached-entity-mid->notable-type assoc mid notable-type)
    notable-type))

(defn get-property-type [property]
  (second (re-find #"(\/.*?\/.*?)\/.*?" property)))

(def cached-fb-property->expected-output-type (e/file-atom {} "freebase-caches/fb-property-to-expected-output-type.edn"))

(defn property->expected-output-type [property]
  (if-let [res (@cached-fb-property->expected-output-type property)]
    res
    (do #_(println "Freebase API call: requesting expected output type for" property)
        (let [property-type (get-property-type property)
              expected-type (->> (mql-read 
                                  [{:id property-type
                                    :/type/type/properties [{:id property
                                                             :type :/type/property
                                                             :expected_type nil}]}])
                                 :result first 
                                 :/type/type/properties first :expected_type)]
          (when (nil? expected-type)
            (throw+ "Got nil from Frebase API"))
          (e/swap! cached-fb-property->expected-output-type assoc property expected-type)
          expected-type))))

(def base-mappings
  {"/people/person" "PERSON"
   "/organization/organization_founder" "PERSON"
   "/organization/organization" "ORGANIZATION"
   "/sports/sports_team" "ORGANIZATION"
   "/business/employer" "ORGANIZATION"
   "/business/business_operation" "ORGANIZATION"
   "/location/location" "LOCATION"
   "/organization/organization_scope" "LOCATION"})

(def cached-fb-included-types (e/file-atom {} "freebase-caches/fb-included-types.edn"))

(defn get-included-types [type]
  (if-let [res (@cached-fb-included-types type)]
    res
    (let [included-types (->> (mql-read 
                               [{:id type
                                 :/freebase/type_hints/included_types [{}]}])
                              :result first 
                              :/freebase/type_hints/included_types (map :id))]
      (e/swap! cached-fb-included-types assoc type included-types)
      included-types)))

(defn type->coarse-type [type]
  (if-let [mapped-type (base-mappings type)]
    mapped-type
    (if-let [included-base-type (first (filter (set (keys base-mappings)) (get-included-types type)))]
      (base-mappings included-base-type)
      "OTHER")))

