(ns day2.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day2/input.txt"
       slurp
       string/split-lines
       (map #(string/split % #" "))))

(def score-map
  {;; indexed by elf shape then our shape OR match outcome
   ;; loses, draws, and wins are 0, 3, and 6 points respectively
   ;; and our usage of rock, paper, scissors are 1, 2, 3 points respectively
   :rock     {:rock     (+ 1 3) :draw (+ 1 3)
              :paper    (+ 2 6) :win  (+ 2 6)
              :scissors (+ 3 0) :lose (+ 3 0)}
   :paper    {:rock     (+ 1 0) :lose (+ 1 0)
              :paper    (+ 2 3) :draw (+ 2 3)
              :scissors (+ 3 6) :win  (+ 3 6)}
   :scissors {:rock     (+ 1 6) :win  (+ 1 6)
              :paper    (+ 2 0) :lose (+ 2 0)
              :scissors (+ 3 3) :draw (+ 3 3)}})

(def elf-letter->shape
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def our-letter->shape
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(defn calculate-score-p1 [[elf-hand our-hand]]
  (->> score-map
       ((elf-letter->shape elf-hand))
       ((our-letter->shape our-hand))))

(defn part1 []
  (->> input
       (map calculate-score-p1)
       (reduce +)))

(def our-letter->outcome
  {"X" :lose
   "Y" :draw
   "Z" :win})

(defn calculate-score-p2 [[elf-hand our-hand]]
  (->> score-map
       ((elf-letter->shape elf-hand))
       ((our-letter->outcome our-hand))))

(defn part2 []
  (->> input
       (map calculate-score-p2)
       (reduce +)))

(comment
  (take 5 input)
  ;; => (["C" "Z"] ["C" "Z"] ["A" "X"] ["A" "X"] ["B" "Z"])
  (part1)
  ;; => 14264
  (part2)
  ;; => 12382
  )
