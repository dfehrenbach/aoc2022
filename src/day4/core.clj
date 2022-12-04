(ns day4.core
  (:require [clojure.string :as string]))

(defn read-range [s]
  (mapv parse-long (string/split s #"-")))

(def input
  (->> "src/day4/input.txt"
       slurp
       string/split-lines
       (map #(string/split % #","))
       (map #(map read-range %))))

(defn range-contains? [[[start1 end1] [start2 end2]]]
  (or (and (<= start1 start2) (<= end2 end1))
      (and (<= start2 start1) (<= end1 end2))))

(defn part1 []
  (->> input
       (filter range-contains?)
       count))

(defn range-overlaps? [[[start1 end1] [start2 end2]]]
  (<= (max start1 start2)
      (min end1 end2)))

(defn part2 []
  (->> input
       (filter range-overlaps?)
       count))

(comment
  (take 3 input)
  ;; => (([33 62] [26 62]) ([49 89] [49 88]) ([2 4] [3 92]))
  (part1)
  ;; => 588
  (part2)
  ;; => 911
  )
