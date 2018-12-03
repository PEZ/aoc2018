
;; Advent of Code, 2018 - https://adventofcode.com/2018
;; Hoppy's solutions: clojurescript on node.js

(ns aoc.core
  (:require [cljs.nodejs :as nodejs]
            [cljs-node-io.core :as io]
            [clojure.string :as cstr]
            [cljs.test :refer-macros [deftest is testing run-tests]]))

(nodejs/enable-util-print!)

(defn- slurp-lines [file-name]
  (cstr/split-lines (io/slurp file-name)))

(defn- daily-data [day-num]
  (let [path (str "data/day" day-num "/input.txt")]
    (slurp-lines path)))

;;===============================================================================
;;=============================== day 1 =========================================
;;===============================================================================

(defn day1-data
  "snarf data for day1 puzzles"
  []
  (map #(js/parseInt %) (daily-data 1)))

(defn day1a
  "find the final frequency by applying changes"
  []
  (apply + (day1-data)))

(defn day1b
  "continously feed changes until we find a repeated frequency"
  []
  (loop [freq 0 seen #{} data (cycle (day1-data))]
    (let [datum (first data)
          freq-next (+ datum freq)]
      (if (seen freq-next)
        freq-next
        (recur freq-next (conj seen freq-next) (rest data))))))

;;===============================================================================
;;=============================== day 2 =========================================
;;===============================================================================

(defn- day2-data
  []
  (daily-data 2))

(defn- has-n-matching
  "see if `astr` has `num` occurances of some character"
  [num astr]
  (some #(= num %) (vals (frequencies astr))))

(defn day2a
  "compute a checksum by product of count of items with 2 and 3 matching chars"
  []
  (let [match-count (fn [n data] (count (filter (partial has-n-matching n) data)))
        data (day2-data)
        n2 (match-count 2 data)
        n3 (match-count 3 data)]
    (* n2 n3)))

(defn- find-duplicates
  "find duplicate strings in collection"
  [scoll]
  (-> (reduce
       (fn [state v]
         (update state (if (contains? (:seen state) v) :dups :seen) conj v))
       {:seen #{} :dups #{}} scoll)
      (:dups)
      (seq)))

(defn- drop-nth
  "drop the `n`th character from a string"
  [n coll]
  (apply str (keep-indexed #(if (not= % n) %2) coll)))

(defn day2b
  "find the strings which differ by only one character, and return result with that char missing"
  []
  (let [data (day2-data)
        dups-with-char-removed (fn [n] (find-duplicates (map #(drop-nth n %) data)))
        mx-len (reduce max (map count data))]
    (->> (map dups-with-char-removed (range mx-len))
         (remove nil?)
         (ffirst))))


;;===============================================================================
;;=============================== day 3 =========================================
;;===============================================================================
(defn- day3-data
  "parse day 3 data -> ({:id :x :y :w :h} ... )"
  []
  (let [line-re #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$"]
    (map (fn [line]
           (when-let [matches (re-matches line-re line)]
             (zipmap [:id :x :y :w :h] (map #(js/parseInt %) (rest matches)))))
         (daily-data 3))))

(defn- cells-touched
  "yields seq of the fabric [x y] values touched by a swath"
  [swath]
  (let [{:keys [x y w h]} swath]
    (for [xx (range x (+ x w))
          yy (range y (+ y h))] [xx yy])))

(defn- reserve-cells
  "adds reservations to the fabric (list of reserving id per cell)
   result is map on [x y] -> vector of occupying reservation id"
  [fabric id cells]
  (persistent!
   (reduce (fn [t-fb xy]
             (assoc! t-fb xy (conj (or (get t-fb xy) []) id))) (transient fabric) cells)))

(defn- reserve-swaths
  "add all the reservations in the dataset to an empty fabric"
  [data]
  (reduce (fn [fb sw]
            (reserve-cells fb (:id sw) (cells-touched sw))) {} data))

(defn- swath-alone?
  "given the reservations, and a swath, tell me if it is unoccupied by others"
  [fb sw]
  (let [cells (cells-touched sw)]
    (= 1 (reduce max
                 (map #(count (get fb %)) cells)))))

(defn day3 []
  (let [data (day3-data)
        reservations (reserve-swaths data)
        over-committed (count (filter (fn [[k v]] (> (count v) 1)) reservations))
        unscathed (first (filter #(swath-alone? reservations %) data))]
    {:over-committed over-committed
     :unscathed unscathed}))

;;=============================================================================

(deftest aoc-tests
  (is (= (day1a) 445))
  (is (= (day1b) 219))
  
  (is (= (day2a) 7872))
  (is (= (day2b) "tjxmoewpdkyaihvrndfluwbzc"))

  (let [ans (day3)]
    (is (= (:over-committed ans) 112418))
    (is (= (-> ans :unscathed :id) 560))))


(defn main [& args]
  (println "hello AOC-2018"))

(set! *main-cli-fn* main)
