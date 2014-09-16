(ns omnivore-experiments.core
  (:use plumbing.core
        fipe.core
        fipe.util
        omnivore-experiments.util)
  (:require [clojure.string :as str]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clj-progress.core :as progress]
            [clojure.tools.logging :as log]
            [omnivore-experiments.aida :as aida]
            [omnivore-experiments.figer :as figer]
            [omnivore-experiments.freebase :as fb]
            [omnivore-experiments.multir :as multir]
            [omnivore-experiments.uclassify :as uclassify]
            [bigml.sampling [reservoir :as reservoir]]
            [com.ashafa.clutch :as clutch]))

(set-fipe-dir! "data")

(progress/set-progress-bar! ":header [:bar] :percent :done/:total :etas")

;; Convert AIDA YAGO2 docs to JSON
(deftarget "aida-yago2-docs.json"
  (aida/dataset->maps (dep "AIDA-YAGO2-dataset.tsv")))

;; Sample recent news articles
(def pnews-files   
  (->> (fs/glob "/projects/pardosa/s2/clzhang/parallelnews/exp15/broil/201407*")
       (sort-by str) (map #(.listFiles %)) aconcat))
(deftarget "parallelnews/2014-07-sample1.json"
  {:with-progress ["Files processed" #(count pnews-files)]}
  (->> pnews-files
       (map #(progress/tick (read-from-file % :as "json"))) aconcat ; => articles
       (filter #(< 20 (count (:text %)))) ; filter out short or empty articles
       (#(reservoir/sample % 3000))))

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

(defn gen-figer-type-rel-map [target-relations-types-file]
  (let [target-relations-types (into [] (read-from-file target-relations-types-file))]
    (let [entries (for [[rel allowed-types] target-relations-types
                        :let [{:keys [in-type out-type]} (first allowed-types)]]
                    {:rel rel
                     :figer-typesig (mapv figer/freebase-type->figer-type [in-type out-type])})]
      (for [[figer-typesig entries] (sort-by first (group-by :figer-typesig entries))]
        (concat figer-typesig (map :rel entries))))))

(deftarget "knowledge-bases/figer-type-rel-map.tsv"
  (gen-figer-type-rel-map
   (dep "knowledge-bases/target-relations-types.edn")))

(defn generate-filler-answers-by-ner [type-sig->rels target-relation->display-name args answers]
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

(defn generate-filler-answers-by-freebase-types [target-relations-types target-relation->display-name args answers]
  (let [filler-count (max (- 4 (count answers)) 2)
        [arg1-types arg2-types] (for [{:keys [mid ner-type] :as arg} args]
                                  (if mid
                                    (fb/entity-mid->type-set (:mid arg))
                                    (if-let [t (multir/ner-type->fb-type ner-type)]
                                      t
                                      (constantly true))))
        possible-relations (for [[relation allowed-types] target-relations-types
                                 :when (some (fn [{:keys [in-type out-type]}]
                                               (and (arg1-types in-type)
                                                    (arg2-types out-type)))
                                             allowed-types)] 
                             relation)
        ;; Remove already used relations:
        possible-relations (filter (complement (set (map :relation answers))) possible-relations)
        filler-relations (take filler-count (shuffle possible-relations))]
    (for [r filler-relations]
      {:relation r
       :relation-display-name (target-relation->display-name r)
       :source ["Filler"]})))

(defn make-questions [target-relations-display-names-file coarse-type-rel-map-file target-relations-types-file relinsts-file]
  (let [target-relation->display-name (into {} (map vec (read-from-file target-relations-display-names-file)))
        type-sig->rels (->> coarse-type-rel-map-file read-from-file
                            (mapv (partial split-at 2)) (mapv #(mapv vec %)) (into {}))
        target-relations-types (into [] (read-from-file target-relations-types-file))
        relinsts-by-sent-inst 
        (->> relinsts-file
             read-from-file first
             (group-by sent-inst-key) (map second)
             (sort-by (comp sent-inst-key first)))]
    (for [relinsts relinsts-by-sent-inst
          :let [answers (vec (for [[rel
                                    relinsts-by-rel] (group-by :relation relinsts)]
                               {:relation rel
                                :relation-display-name (target-relation->display-name rel)
                                :source (mapv :source relinsts-by-rel)
                                :score (mapv :score relinsts-by-rel)}))
                filler-answers (generate-filler-answers-by-freebase-types target-relations-types target-relation->display-name (:args (first relinsts)) answers)
                #_(generate-filler-answers-by-ner type-sig->rels target-relation->display-name (:args (first relinsts)) answers)]]
      (assoc (select-keys (first relinsts) [:doc-name :args])
        :answers (shuffle (concat answers filler-answers))
        :type "Question"))))

;; Make questions
(deftarget "aida-yago2-docs-questions.json"
  (make-questions
   (dep "knowledge-bases/target-relations-display-names.tsv")
   (dep "knowledge-bases/coarse-type-rel-map.tsv")
   (dep "knowledge-bases/target-relations-types.edn")
   (dep "aida-yago2-docs-relinsts-combined.json")))

(defn filter-questions-to-rels [new-answers target-relations-display-names-file questions-file]
  (let [rels (map :relation new-answers)]
    (for [{:keys [answers] :as question} (first (read-from-file questions-file))
          :when (some (set rels) (map :relation answers))]
      (assoc question :answers new-answers))))

(def perloc-answers
  [{:relation "/people/person/nationality"
    :relation-display-name "has nationality"}
   {:relation "/people/person/place_of_birth"
    :relation-display-name "was born in"}
   {:relation "/people/person/places_lived|/people/place_lived/location"
    :relation-display-name "lived in"}
   {:relation "/people/deceased_person/place_of_death"
    :relation-display-name "died in"}])

(deftarget "aida-yago2-docs-questions-perloc.json"
  (filter-questions-to-rels 
   perloc-answers
   (dep "knowledge-bases/target-relations-display-names.tsv")
   (dep "aida-yago2-docs-questions.json")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use data from Ce Zhang, et al. - ACL 2012

(defn get-relinsts-from-batch [files-glob]
  (let [data-files (fs/glob files-glob)
        all-rows (for [dfile data-files
                       :let [header (vec (first (read-from-file dfile)))]
                       row (rest (read-from-file dfile))]
                   (zipmap header row))]
    (for [row all-rows
          num (range 1 6)
          :let [relinst (zipmap [:fact-display :sentence :relation :answer :worker-id] 
                                (conj 
                                 (vec (for [basename ["Input.Relation" "Input.Sentence" "Input.rel" "Answer.Category"]] 
                                        (get row (str basename num))))
                                 (get row "WorkerId")))]
          :when (and (every? identity (vals relinst)) (not= "" (:answer relinst)))]
      relinst)))

(deftarget "ce-zhang-acl-12/relinsts.edn.gz"
  (get-relinsts-from-batch (dep "ce-zhang-acl-12/Batch*")))

(deftarget "ce-zhang-acl-12/num-answers-hist.tsv"
  (->> (for [[relinst anns] (group-by (juxt :fact-display :sentence :relation)
                                      (read-from-file (dep "ce-zhang-acl-12/relinsts.edn.gz")))
             :let [anns (distinct-by :worker-id anns)]]
         (count anns))
       frequencies
       (sort-by first)))

(defn get-agreed-relinsts
  "Get the relation instances that have agreement x/(x+1)?"  
  [relinsts-file x]
  (for [[relinst anns] (group-by (juxt :fact-display :sentence :relation)
                                 (read-from-file relinsts-file))
        :let [anns (distinct-by :worker-id anns)
              by-answer (group-by :answer anns)
              max-agreement (apply max (map (comp count second) by-answer))]
        :when (and (>= (count anns) (inc x))
                   (>= max-agreement x))]
    (assoc (zipmap [:fact-display :sentence :relation] relinst) 
      :answer (first (apply max-key (comp count second) by-answer)))))

(defn num-groups-agree
  "How many relation instances have agreement x/(x+1)?"
  [relinsts-file x]
  (count (get-agreed-relinsts relinsts-file x)))

(deftarget "ce-zhang-acl-12/instances-vs-agreement.tsv"
  (for [x (range 1 12)]
    [(/ x (inc x)) (num-groups-agree (dep "ce-zhang-acl-12/relinsts.edn.gz") x)]))

(deftarget "ce-zhang-acl-12/sample-10-11-agreement.csv"
  (for [relinst (take 100 (shuffle (get-agreed-relinsts (dep "ce-zhang-acl-12/relinsts.edn.gz") 10)))]
    ((juxt :fact-display :sentence :relation :answer) relinst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mechanical Turk Results

(defn question->info [question]
  (let [sent_idx (-> question :args first :sent_idx)
        document (read-from-file (str "data/aida-yago2-docs-preprocessed/" (:doc_name question) ".ser.gz"))
        sentence (nth (multir/get-sentences document) sent_idx)]
    {:sentence (str sentence) :args_text (vec (for [arg (:args question)]
                                                (if (:mention_text arg)
                                                  (:mention_text arg)
                                                  (:text arg))))}))

(defn question_id->info [question_id]
  (let [question (clutch/get-document "praxeng2_development" question_id)]
    (question->info question)))

(def amt-id-regex #"A[A-Z0-9]+")

(defn get-praxeng-mturk-results []
  (let [mturk_anns
        (->> (clutch/all-documents "praxeng2_development" {:include_docs true})
             (map :doc)
             (filter #(= "Annotation" (:type %)))
             (filter #(re-matches amt-id-regex (str/trim (:user_id %))))
             (sort-by :user_id))]
    (for [{:keys [question_id] :as ann} mturk_anns
          :let [question-info (question_id->info question_id)]]
      (merge ann question-info))))

(deftarget "praxeng-mturk-results.csv"
  (let [mturk_anns
        (->> (clutch/all-documents "praxeng2_development" {:include_docs true})
             (map :doc)
             (filter #(= "Annotation" (:type %)))
             (filter #(re-matches amt-id-regex (:user_id %)))
             (sort-by :user_id))]
    (for [{:keys [user_id question_id updated_at response]} mturk_anns
          :let [{:keys [sentence args_text]} (question_id->info question_id)]]
      [user_id 
       (->> response (map :relation_display_name) 
            (str/join " & "))
       sentence
       args_text
       updated_at
       question_id])))

(deftarget "praxeng-perloc-questions.edn.gz"
  (for [question (->>
                  (clutch/all-documents "praxeng2_development" {:include_docs true})
                  (map :doc)
                  (filter #(= "Question" (:type %))))] 
    (merge question (question->info question))))

(deftarget "praxeng-perloc-answers-byquestion.edn.gz"
  (let [questions (read-from-file (dep "praxeng-perloc-questions.edn.gz"))
        qid->qidx (into {} (map vector (map :_id questions) (range)))
        annotations (->> (clutch/all-documents "praxeng2_development" {:include_docs true})
                         (map :doc)
                         (filter #(= "Annotation" (:type %))))]
    (->> (group-by (comp qid->qidx :question_id) annotations)
         vec
         (sort-by first))))

(deftarget "praxeng-perloc-answer-counts.csv"
  (for [[qidx answers] (read-from-file (dep "praxeng-perloc-answers-byquestion.edn.gz"))
        :let [valid-mturk-answers (filter #(re-matches amt-id-regex (str/trim (:user_id %))) answers)]]
    [qidx (count valid-mturk-answers) (count answers)]))

(deftarget "praxeng-mturk-5ans-set.edn.gz"
  (->> (get-praxeng-mturk-results)
       (group-by :question_id)
       (filter #(>= (count (second %)) 5))))

(defn str-response [answer]
  (->> answer :response (map :relation_display_name)
       sort (str/join " & ")))

(deftarget "praxeng-mturk-5ans-agree.csv"
  (for [[question_id answers] (read-from-file (dep "praxeng-mturk-5ans-set.edn"))
        :let [str-answer (->> answers (sort-by :created_at) reverse (take 5)
                              (map str-response)
                              frequencies ;seq (sort-by second) last first
                              )]]
    [question_id (:sentence (first answers)) (:args_text (first answers)) str-answer]))

(deftarget "praxeng-mturk-5ans-gold40.csv"
  (for [[question_id [{:keys [sentence args_text]} & _]] 
        (take 40 (shuffle (read-from-file (dep "praxeng-mturk-5ans-set.edn"))))]
    [question_id sentence args_text]))

(defn eliminate-workers-using-gold [gold-file mturk-ans-file num-control threshold]
  (let [gold (read-from-file gold-file)
        control-ids (take num-control gold-file)
        mturk-ans (->> (read-from-file mturk-ans-file)
                       (filter #((set control-ids) (first %))))
        id->gold (into {} (for [[id _ _ str-answer] gold
                                :let [answer (str/split str-answer #" & ")
                                      answer (if (= [""] answer) [] answer)
                                      answer (set answer)]]
                            [id answer]))
        by-worker (->> mturk-ans
                       (map second) aconcat
                       (group-by :user_id))
        removed-workers (for [[worker-id answers] by-worker
                              :let [num-correct (count
                                                 (for [answer answers
                                                       :let [gold-rels (id->gold (:question_id answer))
                                                             response-rels (->> answer :response (map :relation_display_name) set)]
                                                       :when (= gold-rels response-rels)]
                                                   1))]
                              :when (< (/ num-correct (count answers)) threshold)]
                          worker-id)]
    removed-workers))

(deftarget "praxeng-mturk-removed-workers.edn"
  (eliminate-workers-using-gold (dep "praxeng-mturk-5ans-gold40.csv") (dep "praxeng-mturk-5ans-set.edn") 20 0.5))

(defn str-answer->set [str-answer]
  (let [answer (str/split str-answer #" & ")
        answer (if (= [""] answer) [] answer)
        answer (set answer)]
    answer))

(defn test-against-gold [gold-file mturk-ans-file removed-workers-file]
  (let [gold (read-from-file gold-file)
        id->gold (into {} (for [[id _ _ str-answer] gold
                                :let [answer (str/split str-answer #" & ")
                                      answer (if (= [""] answer) [] answer)
                                      answer (set answer)]]
                            [id answer]))
        removed-workers (set (read-from-file removed-workers-file))]
    (for [[question_id answers] (read-from-file mturk-ans-file)
          :when (id->gold question_id)
          :let [answers (filter #((complement removed-workers) (:user_id %)) answers)
                str-answer (->> answers (sort-by :created_at) reverse (take 5)
                                (map str-response)
                                frequencies 
                                )
                final-str-answer (->> str-answer
                                      seq (sort-by second) last first)]]
      [question_id 
       (:sentence (first answers))
       (:args_text (first answers))
       str-answer
       (id->gold question_id)
       (if (= (id->gold question_id) (str-answer->set final-str-answer))
         1 0)])))

(deftarget "praxeng-mturk-test.csv"
  (test-against-gold (dep "praxeng-mturk-5ans-gold40.csv") 
                     (dep "praxeng-mturk-5ans-set.edn") 
                     (dep "praxeng-mturk-removed-workers.edn")))
