(ns aoc.core
  (:require [cljs.nodejs :as nodejs]
            [cljs-node-io.core :as io]
            [clojure.string :as cstr]))

(nodejs/enable-util-print!)

(defn- slurp-lines [file-name]
  (cstr/split-lines (io/slurp file-name)))

;;=============================================================================

(defn day1-data
  "snarf data for day1 puzzles"
  []
  (map #(js/parseInt %) (slurp-lines "data/day1/input.txt")))

(defn day1a
  "find the final frequency by applying change string"
  []
  (apply + (day1-data)))

(defn day1b
  "continously feed change string until we find a repeated frequency"
  []
  (loop [freq 0 seen #{} data (cycle (day1-data))]
    (let [datum (first data)
          freq-next (+ datum freq)]
      (if (seen freq-next)
        {:repeated-freq freq-next :distinct-count (count seen)}
        (recur freq-next (conj seen freq-next) (rest data))))))

;;=============================================================================
  
(defn main [& args]
  (println "hello AOC-2018"))

(set! *main-cli-fn* main)
