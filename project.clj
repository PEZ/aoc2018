(defproject aoc2018 "0.1.0-SNAPSHOT"
  :description "Advent of Code in CLJS"
  :url "http://gnurdle.com.com"

  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/clojurescript "1.10.439"]
                 [org.clojure/tools.reader "1.3.2"]
                 [cljs-node-io "1.1.2"]]

  :jvm-opts ^:replace ["-Xmx1g" "-server"]

  :profiles {:dev {:source-paths ["src"]}}

  :plugins [[lein-pprint "1.1.2"]
            [lein-cljsbuild "1.1.7"]
            [lein-ancient "0.6.15"]]

  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src"]
     :compiler {:main aoc.core
                :verbose false
                :output-dir "bin"
                :output-to "bin/aoc.js"
                :target :nodejs
                :source-map true
                :optimizations :none
                :cache-analysis true
                :parallel-build true}}]})

;; Calva custom cljs repl snippet:
;;
;;    "calva.customCljsRepl": {
;;        "name" : "Node",
;;        "startCode" : "(do (require '[cljs.repl.node]) (require '[cider.piggieback]) (cider.piggieback/cljs-repl (cljs.repl.node/repl-env)))",
;;        "connectedRegExp": "To quit, type: :cljs/quit"
;;    }


