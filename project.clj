(defproject com.biutthapa/vary "0.0.1-SNAPSHOT"
  :description "Elegant variant types for Clojure"
  :url "https://github.com/biutthapa/vary"
  :license {:name "MIT"
            :url "https://github.com/docopt/docopt.clj/blob/master/LICENSE"}
  :source-paths ["src"]
  :test-paths ["test"]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [metosin/malli "0.17.0"]]
  :profiles
  {:test {:dependencies [[org.clojure/test.check "1.1.1"]
                         [io.github.cognitect-labs/test-runner "0.5.1"]]}
   :uberjar {:aot :all}}
  :aliases
  {"test" ["run" "-m" "cognitect.test-runner" "-d" "test"]}
  :repositories [["clojars" "https://repo.clojars.org/"]])
