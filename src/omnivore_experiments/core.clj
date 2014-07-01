(ns omnivore-experiments.core
  (:use plumbing.core
        fipe.core
        fipe.util
        omnivore-experiments.util)
  (:require [clojure.string :as str]
            [omnivore-experiments.aida :as aida]
            [omnivore-experiments.multir :as multir]))

(set-fipe-dir! "data")

;; Ready to import into CouchDB
(deftarget "aida-yago2-docs.json"
  (aida/dataset->maps (dep "AIDA-YAGO2-dataset.tsv")))

;; Preprocess using Stanford NLP
(deftarget! "aida-yago2-docs-preprocessed"
  (multir/preprocess-docs *target* (dep "aida-yago2-docs.json")))

;; KB Matching
(deftarget "aida-yago2-docs-kb-relinsts.json"
  (multir/extract-relinsts-by-kb-matching-bulk 
   (dep "knowledge-bases/kb-facts.tsv.gz")
   (dep "freebase/entity-names.tsv.gz")
   (dep "knowledge-bases/target-relations.tsv")
   (dep "aida-yago2-docs-preprocessed")))

;; MultiR extraction
(deftarget "aida-yago2-docs-multir-relinsts.json"
  (multir/extract-relinsts-bulk
   (dep "multir-extractor-nel+tc+tp")
   (dep "knowledge-bases/target-relations-types.edn")
   (dep "aida-yago2-docs-preprocessed")))

(def sent-inst-key (juxt #(->> % :doc-name multir/doc-name->number) 
                         #(->> % :args (mapv (juxt :sent-idx :sent-tok-span)))))

(deftarget "aida-yago2-docs-relinsts-combined.json"
  (->> [(dep "aida-yago2-docs-kb-relinsts.json")
        (dep "aida-yago2-docs-multir-relinsts.json")]
       (map read-from-file) (map first) aconcat
       (sort-by sent-inst-key)))

(defn generate-filler-answers [type-sig->rels target-relation->display-name args answers]
  (let [filler-count (max (- 4 (count answers)) 2)
        arg-types (vec (for [arg args
                             :let [t (:ner-type arg)]]
                         (if (= t "MISC") "OTHER" t)))
        possible-relations (if (= [nil nil] arg-types)
                             (aconcat (vals type-sig->rels))
                             (if (nil? (first arg-types))
                               (for [[[t1 t2] rels] type-sig->rels
                                     :when (= t2 (second arg-types))
                                     r rels]
                                 r)
                               (if (nil? (second arg-types))
                                 (for [[[t1 t2] rels] type-sig->rels
                                       :when (= t1 (first arg-types))
                                       r rels]
                                   r)
                                 (get type-sig->rels arg-types (aconcat (vals type-sig->rels))))))
        possible-relations (filter (complement (set (map :relation answers))) possible-relations)
        filler-relations (take filler-count (shuffle possible-relations))]
    (for [r filler-relations]
      {:relation r
       :relation-display-name (target-relation->display-name r)
       :source ["Filler"]})))

(defn make-questions [target-relations-display-names-file coarse-type-rel-map-file relinsts-file]
  (let [target-relation->display-name (into {} (map vec (read-from-file target-relations-display-names-file)))
        type-sig->rels (->> coarse-type-rel-map-file read-from-file
                            (mapv (partial split-at 2)) (mapv #(mapv vec %)) (into {}))
        relinsts-by-sent-inst 
        (->> relinsts-file
             read-from-file first
             (group-by sent-inst-key) (map second)
             (sort-by (comp sent-inst-key first)))]
    (for [relinsts relinsts-by-sent-inst
          :let [answers (vec (for [[rel relinsts-by-rel] (group-by :relation relinsts)]
                               {:relation rel
                                :relation-display-name (target-relation->display-name rel)
                                :source (mapv :source relinsts-by-rel)
                                :score (mapv :score relinsts-by-rel)}))
                filler-answers (generate-filler-answers type-sig->rels target-relation->display-name (:args (first relinsts)) answers)]]
      (assoc (select-keys (first relinsts) [:doc-name :args])
        :answers (shuffle (concat answers filler-answers))))))

;; Make questions
(deftarget "aida-yago2-docs-questions.json"
  (make-questions
   (dep "knowledge-bases/target-relations-display-names.tsv")
   (dep "knowledge-bases/coarse-type-rel-map.tsv")
   (dep "aida-yago2-docs-relinsts-combined.json")))
