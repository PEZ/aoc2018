(defproject aoc2018 "0.1.0-SNAPSHOT"
  :description "Advent of Code in CLJS"
  :url "//http://paradoxescaperooms.com"

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.439"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/tools.reader "1.3.2"]
                 [cljs-node-io "1.1.2"]
                 [figwheel-sidecar "0.5.17"]
                 [cider/piggieback "0.3.10"]]

  :jvm-opts ^:replace ["-Xmx1g" "-server"]

  :profiles {:dev {:source-paths ["src"]}}

  :plugins [[lein-pprint "1.1.2"]
            [lein-cljsbuild "1.1.7"]
            [lein-ancient "0.6.15"]]

  :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}

  :cljsbuild
  {:builds
   [{:id "aoc"
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

