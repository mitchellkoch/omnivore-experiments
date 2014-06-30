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
(deftarget "aida-yago2-docs-kb-relinsts.json"
  (wharf/transform-keys 
   (comp wharf/dash->underscore name)
   (multir/extract-relinsts-by-kb-matching-bulk 
    (dep "knowledge-bases/kb-facts.tsv.gz")
    (dep "freebase/entity-names.tsv.gz")
    (dep "knowledge-bases/target-relations.tsv")
    (dep "aida-yago2-docs-preprocessed"))))

;; MultiR extraction
(deftarget "aida-yago2-docs-multir-relinsts.json"
  (wharf/transform-keys 
   (comp wharf/dash->underscore name)
   (multir/extract-relinsts-bulk
    (dep "multir-extractor-nel+tc+tp")
    (dep "knowledge-bases/target-relations-types.edn")
    (dep "aida-yago2-docs-preprocessed"))))

(def sent-inst-key (juxt #(->> % :doc-name multir/doc-name->number) 
                         #(->> % :args (mapv (juxt :sent-idx :sent-tok-span)))))

(deftarget "aida-yago2-docs-relinsts-combined.json"
  (->> [(dep "aida-yago2-docs-kb-relinsts.json")
        (dep "aida-yago2-docs-multir-relinsts.json")]
       (map read-from-file) (map first) aconcat
       (wharf/transform-keys (comp keyword wharf/underscore->dash))
       (sort-by sent-inst-key)
       (wharf/transform-keys (comp wharf/dash->underscore name))))

(defn make-questions [target-relations-display-names-file coarse-type-rel-map-file relinsts-file]
  (let [target-relation->display-name (into {} (map vec (read-from-file target-relations-display-names-file)))
        relinsts-by-sent-inst 
        (->> relinsts-file
             read-from-file first (wharf/transform-keys (comp keyword wharf/underscore->dash))
             (group-by sent-inst-key) (map second)
             (sort-by (comp sent-inst-key first)))]
    (for [relinsts (take 1 relinsts-by-sent-inst)]
      (assoc (select-keys (first relinsts) [:doc-name :args])
        :answers (->> (for [relinst relinsts]
                        (assoc (select-keys relinst [:relation :score :source])
                          :relation-display-name (target-relation->display-name (:relation relinst))))
                      vec shuffle)))))

;; Make questions
(deftarget "aida-yago2-docs-questions.json"
  (wharf/transform-keys 
   (comp wharf/dash->underscore name)
   (make-questions
    (dep "knowledge-bases/target-relations-display-names.tsv")
    (dep "knowledge-bases/coarse-type-rel-map.tsv")
    (dep "aida-yago2-docs-relinsts-combined.json"))))
