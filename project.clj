(defproject music-mining "0.1.0-SNAPSHOT"
  :description "A project for mining data for music."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [nz.ac.waikato.cms.weka/weka-stable "3.6.10"]
                 [commons-io/commons-io "2.4"]]
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :resource-paths ["resources"]
)
