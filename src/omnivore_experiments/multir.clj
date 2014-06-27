(ns omnivore-experiments.multir
  (:use plumbing.core
        fipe.util
        omnivore-experiments.util
        [slingshot.slingshot :only [try+ throw+]])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [wharf.core :as wharf]
            iroh.core
            [omnivore-experiments.freebase :as fb])
  (:import edu.stanford.nlp.util.CoreMap
           edu.stanford.nlp.ling.CoreLabel
           [java.util Properties List ArrayList]
           [edu.stanford.nlp.pipeline Annotation StanfordCoreNLP]
           [edu.stanford.nlp.ie.machinereading.structure Span AnnotationUtils EntityMention]
           [edu.washington.multir.preprocess CorpusPreprocessing]
           [edu.washington.multirframework.corpus SentNamedEntityLinkingInformation$NamedEntityLinkingAnnotation SentFreebaseNotableTypeInformation$FreebaseNotableTypeAnnotation
            TokenOffsetInformation$SentenceRelativeCharacterOffsetBeginAnnotation
            TokenOffsetInformation$SentenceRelativeCharacterOffsetEndAnnotation]
           [edu.stanford.nlp.util Pair Triple]
           [edu.knowitall.tool.chunk OpenNlpChunker]
           [opennlp.tools.chunker ChunkerME]
           [edu.washington.multirframework.argumentidentification ArgumentIdentification SententialInstanceGeneration RelationMatching NERArgumentIdentification NERRelationMatching DefaultSententialInstanceGeneration NELAndNERArgumentIdentification NELRelationMatching NELAndCorefArgumentIdentification NELAndCorefArgumentIdentification$CorefArgument CorefSententialInstanceGeneration NELArgumentIdentification]
           [edu.washington.multirframework.knowledgebase KnowledgeBase]
           [edu.washington.multir.development Preprocess]
           [edu.washington.multir.sententialextraction DocumentExtractor]
           [edu.washington.multirframework.data Argument KBArgument]))

(defn get-sentences [^Annotation doc]
  (.get doc edu.stanford.nlp.ling.CoreAnnotations$SentencesAnnotation))

(defn get-tokens [^CoreMap sentence]
  (.get sentence edu.stanford.nlp.ling.CoreAnnotations$TokensAnnotation))

(defn ner-tags->arg-info [ner-tags]
  (let [ner-tags (map #(if (= % "O") nil %) ner-tags)
        ner-tags-indexed (map vector (range) ner-tags)
        ner-args-partitioned (partition-by second ner-tags-indexed)
        ner-args-partitioned-nonones (filter #(second (first %)) ner-args-partitioned)
        arg-info (for [arg ner-args-partitioned-nonones] 
                   [(ffirst arg) 
                    (inc (first (last arg)))
                    (nth ner-tags (ffirst arg))])]
    arg-info))

(defn sentence-ner-mentions [^CoreMap sentence sentidx]
  (let [ner-tags (->> sentence
                      get-tokens
                      (map (fn [^CoreLabel l] (.get l edu.stanford.nlp.ling.CoreAnnotations$NamedEntityTagAnnotation))))]
    (for [[start-tok end-tok tag] (ner-tags->arg-info ner-tags)]
      {:sentidx sentidx
       :sent-tok-span [start-tok end-tok]
       :text (AnnotationUtils/getTextContent sentence (Span. start-tok end-tok))
       :ner-tag tag})))

(defn sent-char-span->sent-tok-span [sentence [start-offset end-offset]]
  (let [start-tok-idx (first
                     (for [[i ^CoreLabel token] (indexed (get-tokens sentence))
                           :when (= start-offset (.get token TokenOffsetInformation$SentenceRelativeCharacterOffsetBeginAnnotation))]
                       i))
        end-tok-idx (first
                   (for [[i ^CoreLabel token] (indexed (get-tokens sentence))
                         :when (= end-offset (.get token TokenOffsetInformation$SentenceRelativeCharacterOffsetEndAnnotation))]
                     (inc i)))]
    [start-tok-idx end-tok-idx]))

(def corenlp-pipeline-for-preprocessor
  (delay
   (let [^Properties props 
         (doto (Properties.) 
           (.put "annotators" "tokenize,ssplit,pos,lemma,ner,parse,dcoref")
           (.put "sutime.binders" "0")
           (.put "tokenize.whitespace" "true")
           (.put "ssplit.eolonly" "true"))]
     (StanfordCoreNLP. props false))))

(def chunker (delay (.chunker (OpenNlpChunker.))))

(defn annotate-chunks! [^CoreMap sentence]
  (let [tokens (get-tokens sentence)
        token-strs (map (fn [^CoreLabel t] (.get t edu.stanford.nlp.ling.CoreAnnotations$TextAnnotation)) tokens)
        pos-strs (map (fn [^CoreLabel t] (.get t edu.stanford.nlp.ling.CoreAnnotations$PartOfSpeechAnnotation)) tokens)
        chunk-strs (into []
                         (try+
                          (.chunk ^ChunkerME @chunker 
                                  ^"[Ljava.lang.String;" (into-array String token-strs)
                                  ^"[Ljava.lang.String;" (into-array String pos-strs))
                          (catch Exception e 
                            (throw+ {:sentence sentence, :token-strs token-strs, :pos-strs pos-strs}))))]
    (doseq [[^CoreLabel token, chunk-str] (map vector (get-tokens sentence) chunk-strs)]
      (.set token edu.stanford.nlp.ling.CoreAnnotations$ChunkAnnotation chunk-str))))

;; Might allow duplicate nelAnnotations if they have different char spans in the original, but their token spans are the same
(defn preprocess-doc [doc-map]
  (iroh.core/apply-element CorpusPreprocessing "pipeline" [CorpusPreprocessing @corenlp-pipeline-for-preprocessor]) ; set private static pipeline field
  (let [doc-obj (CorpusPreprocessing/getTestDocumentFromRawString (:text doc-map) (:doc-name doc-map))]
    (doseq [[sentidx ^CoreMap sentence] (indexed (get-sentences doc-obj))
            :let [sentence-mentions (filter #(= (:sent-idx %) sentidx) (:nel-annotations doc-map))
                  ^List nelInformation (ArrayList.)
                  ^List notableTypeData (ArrayList.)]]
      (annotate-chunks! sentence)
      (doseq [{:keys [sent-tok-span mid]} sentence-mentions
              :let [nelAnnotation (Triple. (Pair. (int (first sent-tok-span))
                                                  (int (second sent-tok-span))) 
                                           (or mid "null")
                                           (float 1) ; assuming this is gold data
                                           )]]
        (.add nelInformation nelAnnotation)
        (when mid
          (let [notable-type (fb/entity-mid->notable-type mid)
                notableTypeAnnotation (Triple. (Pair. (int (first sent-tok-span))
                                                      (int (second sent-tok-span)))
                                               notable-type
                                               mid)]
            (when notable-type (.add notableTypeData notableTypeAnnotation)))))
      (.set sentence SentNamedEntityLinkingInformation$NamedEntityLinkingAnnotation nelInformation)
      (.set sentence SentFreebaseNotableTypeInformation$FreebaseNotableTypeAnnotation notableTypeData))
    doc-obj))

(defn preprocess-docs [out-dir docs-file]
  (fs/mkdirs out-dir)
  (let [docs (first
              (wharf/transform-keys
               (comp keyword wharf/underscore->dash)
               (read-from-file docs-file)))]
    (doseq [doc docs]
      (write-to-file (io/file out-dir (str (:doc-name doc) ".ser.gz")) 
                     (preprocess-doc doc)))))

(defn extract-relinsts-by-kb-matching [kb [doc-name doc]]
  (let [^ArgumentIdentification arg-ident (NELAndNERArgumentIdentification/getInstance)
        ^SententialInstanceGeneration sent-inst-gen (DefaultSententialInstanceGeneration/getInstance)
        ^RelationMatching rel-matching (NELRelationMatching/getInstance)]
    (distinct-fast
     (for [[sentidx sentence] (indexed (get-sentences doc))
           :let [ner-mentions (sentence-ner-mentions sentence sentidx)
                 args (.identifyArguments arg-ident doc sentence)
                 sent-insts (.generateSententialInstances sent-inst-gen args sentence)]
           ^Triple annotation (.matchRelations rel-matching sent-insts kb nil nil)
           ;;annotation sent-insts
           ]
       (array-map 
        :doc-name doc-name
        :relation (.third annotation)
        :args (for [^Argument arg [(.first annotation) (.second annotation)]
                    :let [sent-tok-span (sent-char-span->sent-tok-span sentence [(.getStartOffset arg) (.getEndOffset arg)])]]
                (array-map
                 :sent-idx sentidx
                 :sent-tok-span sent-tok-span
                 :mention-text (.getArgName arg)
                 :mid (when (instance? KBArgument arg) (.getKbId ^KBArgument arg))
                 :arg-from (if (instance? KBArgument arg) "NEL" "NER")
                 :ner-type (->> ner-mentions
                                (filter #(= sent-tok-span (:sent-tok-span %)))
                                first :ner-tag))))))))

(defn extract-relinsts-by-kb-matching-bulk [kb-facts-file kb-entity-names-file target-relations-file docs-dir]
  (let [doc-files (sort-by str (fs/glob (str docs-dir "/*.ser.gz")))
        doc-names (map #(fs/base-name % ".ser.gz") doc-files)
        doc-objs (map read-from-file doc-files)
        docs (map vector doc-names doc-objs)
        kb (KnowledgeBase. (str kb-facts-file) (str kb-entity-names-file) (str target-relations-file))]
    (aconcat (pmap (partial extract-relinsts-by-kb-matching kb) docs))))
