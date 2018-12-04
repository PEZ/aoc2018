
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

;;===============================================================================
;;=============================== day 4 =========================================
;;===============================================================================

(defn- grok-event
  "turn event text into an event"
  [txt]
  (cond
    (= "falls asleep" txt) [:sleeps]
    (= "wakes up" txt) [:wakes]
    :else (let [matches (re-matches #"^Guard #(\d+) begins shift$" txt)]
            (if matches
              [:starts (js/parseInt (second matches))]
              (throw (js/Error. (str "invalid event: " txt)))))))

(defn- day4-data
  "parse day 4 data -> ( :ts {:yr :mon :day :hr :min} :event {} } ... )"
  []
  (let [line-re #"^\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.*)$"]
    (->> (daily-data 4)
         (map (fn [line]
                (when-let [matches (re-matches line-re line)]
                  (conj (into [] (map #(js/parseInt %) (take 5 (rest matches))))
                        (grok-event (last matches))))))
         (sort))))

;; the naplog structure is used to absorb events in time-order, and
;; bin these into slots by ID that can be analyzed.
(defn- empty-naplog
  "an initialized naplog `object`"
  []
  {:on-duty nil         ;; who was last logged on duty
   :start-sleep nil     ;; time sleep started (when sleeping)
   :sleep-histograms {} ;; map id->histogram by minute of being asleep
   })

(defn- update-naplog
  "assuming log-datums are presented in chron order, transition the naplog to accumulate
  histograms of sleep patterns"
  [naplog log-datum]
  (let [[minute evt] (take 2 (drop 4 log-datum))
        ev-type (first evt)]
    
    (case ev-type
      
      ;; new guard started, record
      :starts (assoc naplog :on-duty (second evt))

      ;; current guard starts sleeping, record
      :sleeps (assoc naplog :start-sleep minute)

      ;; current guard wakes up, update histogram
      :wakes  (let [{:keys [on-duty start-sleep]} naplog
                    stop-sleep minute 
                    histogram (get-in naplog [:sleep-histograms on-duty] (into [] (repeat 60 0)))
                    updated-histogram (map-indexed (fn [i v]
                                                     (if (and (>= i start-sleep)
                                                              (<  i stop-sleep))
                                                       (inc v) v)) histogram)]
                (assoc-in naplog [:sleep-histograms on-duty] updated-histogram)))))

(defn- most-slept
  "return the histogram entry for most slept"
  [hists]
  (let [sums (into {} (map (fn [[k v]] [k (apply + v)]) hists))
        best-key (key (apply max-key val sums))]
    {:id best-key :total (get sums best-key) :histogram (get hists best-key)}))

(defn- sleepyest-minute
  "return the histogram entry which has the biggest slept minute"
  [hists]
  (let [max-map (into {} (map (fn [[k v]] [k (apply max v)]) hists))
        best-id (key (apply max-key val max-map))
        best-minute (first (apply max-key second (map-indexed vector (get hists best-id))))]
    {:id best-id :best-minute best-minute}))

(defn day4a []
  (let [naplog (reduce update-naplog (empty-naplog) (day4-data))
        sleep-winner (most-slept (:sleep-histograms naplog))
        id (:id sleep-winner)
        best-minute (first (apply max-key second (map-indexed vector (:histogram sleep-winner))))
        cookie (* id best-minute)]
    {:id id :best-minute best-minute :cookie cookie}))

(defn day4b []
  (let [naplog (reduce update-naplog (empty-naplog) (day4-data))
        sleep-winner (sleepyest-minute (:sleep-histograms naplog))
        id (:id sleep-winner)
        best-minute (:best-minute sleep-winner)
        cookie (* id best-minute)]
    {:id id :best-minute best-minute :cookie cookie}))

;;=============================================================================

(deftest aoc-tests
  (is (= (day1a) 445))
  (is (= (day1b) 219))
  
  (is (= (day2a) 7872))
  (is (= (day2b) "tjxmoewpdkyaihvrndfluwbzc"))

  (let [ans (day3)]
    (is (= (:over-committed ans) 112418))
    (is (= (-> ans :unscathed :id) 560)))

  (is (= (day4a) {:id 3203, :best-minute 44, :cookie 140932})
      (= (day4b) {:id 1601, :best-minute 32, :cookie 51232})))


(defn main [& args]
  (println "hello AOC-2018"))

(set! *main-cli-fn* main)
