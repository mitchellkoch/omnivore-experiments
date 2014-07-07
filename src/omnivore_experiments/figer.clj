(ns ^{:doc "FIGER-related functions"
      :author "Mitchell Koch"}
  omnivore-experiments.figer
  (:use omnivore-experiments.util
        fipe.util
        plumbing.core
        [slingshot.slingshot :only [throw+]])
  (:require [clojure.tools.logging :as log]
            [clojure.string :as str]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]))

(def figer-mapping (delay (->> "data/knowledge-bases/figer-type-map.tsv"
                               read-from-file (mapv vec) (into {}))))

(defn freebase-type->figer-type [freebase-type]
  (if-let [figer-type (get @figer-mapping freebase-type)]
    figer-type
    (log/warn (str "No FIGER mapping for Freebase type " freebase-type))))
