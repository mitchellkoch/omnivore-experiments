(defproject omnivore-experiments "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :repositories [["sonatype-snapshots" {:url "http://oss.sonatype.org/content/repositories/snapshots"}]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [mitchellkoch/fipe "0.1.1-SNAPSHOT"]
                 [prismatic/plumbing "0.3.2"]
                 [wharf "0.1.0-SNAPSHOT"]
                 [cheshire "5.3.1"]
                 [me.raynes/fs "1.4.5"]
                 [http-kit "2.1.16"]
                 [im.chit/iroh "0.1.5"]
                 [alandipert/enduro "1.1.5"]
                 [slingshot "0.10.3"]
                 [org.flatland/ordered "1.5.2"]
                 [org.clojure/tools.logging "0.3.0"]
                 [com.ashafa/clutch "0.4.0-RC1"]
                 
                 ;; For MultirExperiments:
                 [org.apache.commons/commons-io "1.3.2"]
                 [org.apache.commons/commons-lang3 "3.1"]
                 [edu.stanford.nlp/stanford-corenlp "1.3.5"]
                 [edu.washington.cs.knowitall/multir-framework_2.10 "0.1-SNAPSHOT"]
                 [edu.washington.cs.knowitall.stanford-corenlp/stanford-ner-models "1.3.5"]
                 [edu.washington.cs.knowitall.stanford-corenlp/stanford-postag-models "1.3.5"]
                 [edu.washington.cs.knowitall.stanford-corenlp/stanford-dcoref-models "1.3.5"]
                 [edu.washington.cs.knowitall.stanford-corenlp/stanford-parse-models "1.3.5"]
                 [edu.washington.cs.knowitall.stanford-corenlp/stanford-sutime-models "1.3.5"]
                 [org.apache.derby/derby "10.10.1.1"]
                 [edu.washington.cs.knowitall/reverb-core "1.4.3"]
                 [edu.washington.cs.knowitall.nlptools/nlptools-core_2.10 "2.4.4"]
                 [edu.washington.cs.knowitall.nlptools/nlptools-chunk-opennlp_2.10 "2.4.4"]
                 [edu.mit/jwi "2.2.3"]
                 [edu.washington.cs.knowitall.nlptools/nlptools-wordnet-uw_2.10 "2.4.4"]
                 [org.apache.hadoop/hadoop-core "0.20.2"]
                 [postgresql/postgresql "9.0-801.jdbc4"]
                 [com.cedarsoftware/json-io "2.6.0"]
                 [com.google.code.externalsortinginjava/externalsortinginjava "0.1.9"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 [org.slf4j/slf4j-log4j12 "1.7.7"]]
  :aliases {"compile-all" ^{:doc "Run javac appropriately and compile."}
            ["do" ["with-profile" "empty" "javac"] "compile"]}
  :java-source-paths ["../MultirExperiments/src"]
  :jvm-opts ["-Xmx30g" "-server" "-Djava.util.Arrays.useLegacyMergeSort=true" "-XX:-OmitStackTraceInFastThrow" "-XX:MaxPermSize=1g" "-XX:+CMSClassUnloadingEnabled" "-XX:+UseConcMarkSweepGC"]
  :main omnivore-experiments.core
  :profiles {:empty {}}
  :repl-options {:port 4001})
