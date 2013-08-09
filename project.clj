(defproject clojure-playground "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots"
                               :update :always}]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.xml "0.0.7"]
                 [org.clojure/data.zip "0.1.1"]
                 [local/core-async "0.1.0-SNAPSHOT"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/core.typed "0.1.19"]
                 [org.apache.commons/commons-email "1.3.1"]
                 [org.zeromq/cljzmq "0.1.2-SNAPSHOT"]
                 ]
  :jvm-opts ["-Djava.library.path=/usr/lib:/usr/local/lib"] 
  :java-source-paths ["src/java"]
  :main clojure-playground.types
)







