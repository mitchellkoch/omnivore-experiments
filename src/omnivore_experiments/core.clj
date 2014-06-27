(ns omnivore-experiments.core
  (:use plumbing.core
        fipe.core
        fipe.util
        omnivore-experiments.util)
  (:require [clojure.string :as str]
            [omnivore-experiments.aida :as aida]
            [omnivore-experiments.multir :as multir]
            [wharf.core :as wharf]))

(set-fipe-dir! "data")

;; Ready to import into CouchDB
(deftarget "aida-yago2-docs.json"
  (wharf/transform-keys 
   (comp wharf/dash->underscore name)
   (aida/dataset->maps (dep "AIDA-YAGO2-dataset.tsv"))))

;; Preprocess using Stanford NLP
(deftarget! "aida-yago2-docs-preprocessed"
  (multir/preprocess-docs *target* (dep "aida-yago2-docs.json")))

;; KB Matching
(deftarget "aida-yago2-docs-kbrelinsts.json"
  (wharf/transform-keys 
   (comp wharf/dash->underscore name)
   (multir/extract-relinsts-by-kb-matching-bulk 
    (dep "knowledge-bases/kb-facts.tsv.gz")
    (dep "freebase/entity-names.tsv.gz")
    (dep "knowledge-bases/target-relations.tsv")
    (dep "aida-yago2-docs-preprocessed"))))
