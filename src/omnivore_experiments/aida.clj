(ns omnivore-experiments.aida
  (:use plumbing.core
        omnivore-experiments.util
        fipe.util)
  (:require [clojure.string :as str]))

(defn doc-lines->text [doc-lines]
  (str/join "\n"
            (for [sentence-lines (split-on #(= [""] (vec %)) doc-lines)]
              (str/join " " (map first sentence-lines)))))

(defn parse-line [line]
  (zipmap [:text :iob :mention-text :yago2 :wikipedia-url :wikipedia-id :mid] line))

(defn index-tokens 
  ([doc-lines] (index-tokens doc-lines {:char-span [0 0]
                                        :sent-idx 0
                                        :sent-tok-idx 0}))
  ([doc-lines {:keys [char-span sent-idx sent-tok-idx] :as location}]
     (lazy-seq
      (when-let [[line & more] (seq doc-lines)]
        (let [{:keys [text] :as token-info} (parse-line line)]
          (cons (merge (update-in location [:char-span 1] #(+ % (count text))) 
                       token-info)
                (index-tokens more
                              (-> location
                                  (update-in [:char-span 0] #(+ % (inc (count text))))
                                  (update-in [:char-span 1] #(+ % (inc (count text))))
                                  (update-in [:sent-idx] (if (= "" text) inc identity))
                                  (update-in [:sent-tok-idx] (if (= "" text) (constantly 0) inc))))))))))

(defn partition-by-iob [indexed-tokens]
  (lazy-seq
   (when-let [[{:keys [iob] :as token} & more] (seq indexed-tokens)]
     (if (= "B" iob)
       (cons (cons token (take-while #(= "I" (:iob %)) more))
             (partition-by-iob (drop-while #(= "I" (:iob %)) more)))
       (cons [token] (partition-by-iob more))))))

(defn get-nel-annotations [indexed-tokens]
  (for [mention-tokens (->> indexed-tokens 
                            partition-by-iob
                            (filter #(:wikipedia-url (first %))))]
    (merge 
     (select-keys (first mention-tokens) [:mention-text :yago2 :wikipedia-url :wikipedia-id :mid])
     (array-map
      :char-span [(-> mention-tokens first :char-span first)
                  (-> mention-tokens last :char-span last)]
      :sent-idx (-> mention-tokens first :sent-idx)
      :sent-tok-span [(-> mention-tokens first :sent-tok-idx)
                      (inc (-> mention-tokens last :sent-tok-idx))]))))

(defn dataset->maps [dataset-file]
  (let [lines (read-from-file dataset-file)]
    (for [[idx doc-lines] (indexed (split-on #(.startsWith (first %) "-DOCSTART-") lines))
          :let [indexed-tokens (index-tokens doc-lines)]]
      {:doc-name (str "AIDA-YAGO2-DOC" (inc idx))
       :text (doc-lines->text doc-lines)
       :nel-annotations (get-nel-annotations indexed-tokens)})))
