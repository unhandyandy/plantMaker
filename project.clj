(defproject plant-maker "0.1.0-SNAPSHOT"
  :description "create 2 dimensional L-system figures"
  :url "mailto:dabrowsa@indiana.edu"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [seesaw "1.4.5"]
                 [net.mikera/vectorz-clj "0.40.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot plant-maker.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
