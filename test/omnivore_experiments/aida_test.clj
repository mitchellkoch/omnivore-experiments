(ns omnivore-experiments.aida-test
  (:require [clojure.test :refer :all]
            [omnivore-experiments.aida :refer :all]
            [fipe.util :refer :all]
            [clojure.string :as str]))

(deftest parse-line-test
  (is (= {:text "European" :iob "B" :mention-text "European Union" :yago2 "European_Union" :wikipedia-url "http://en.wikipedia.org/wiki/European_Union" :wikipedia-id "9317" :mid "/m/02jxk"}
         (parse-line '("European" "B" "European Union" "European_Union" "http://en.wikipedia.org/wiki/European_Union" "9317" "/m/02jxk"))))

  (is (= {:text "-DOCSTART- (1 EU)"}
         (parse-line '("-DOCSTART- (1 EU)"))))
  
  (is (= {:text "EU" :iob "B" :mention-text "EU" :yago2 "--NME--"}
         (parse-line '("EU" "B" "EU" "--NME--")))))
