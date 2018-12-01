(ns aoc.core
  (:require [cljs.nodejs :as nodejs]))

(nodejs/enable-util-print!)

(defn main [& args]
  (println "hello AOC-2018"))

(set! *main-cli-fn* main)
