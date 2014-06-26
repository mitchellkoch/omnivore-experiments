(ns omnivore-experiments.core
  (:use plumbing.core
        fipe.core
        fipe.util
        omnivore-experiments.util)
  (:require [clojure.string :as str]
            [omnivore-experiments.aida :as aida]
            [wharf.core :as wharf]))

(set-fipe-dir! "data")

(deftarget "aida-yago2-docs.json"
  (wharf/transform-keys 
   (comp wharf/dash->underscore name)
   (aida/dataset->maps (dep "AIDA-YAGO2-dataset.tsv"))))

