(ns omnivore-experiments.util)

(defn split-on [pred coll]
  (lazy-seq
   (when-let [lines (seq coll)]
     (let [item (take-while (complement pred) lines)
           more (split-on pred (rest (drop-while (complement pred) lines)))]
       (if (not (empty? item)) 
         (cons item more)
         more)))))
