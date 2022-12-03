(ns day3.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input
  (->> "src/day3/input.txt"
       slurp
       string/split-lines))

(defn halve-pack [s]
  (split-at (/ (count s) 2) s))

(defn find-intersecting-item [compartments]
  (->> compartments
       (map set)
       (apply set/intersection)))

(defn lowercase? [ch]
  (<= 97 (int ch)))

(defn item->priority
  "Lowercase item types a through z have priorities 1 through 26.
   Uppercase item types A through Z have priorities 27 through 52."
  [ch] (if (lowercase? ch)
         (- (int ch) 96) ;; 97 is \a
         (+ 26 (- (int ch) 64)) ;; 65 is \A
         ))

(defn part1 []
  (->> input
       (map halve-pack)
       (map find-intersecting-item)
       (map (comp item->priority first))
       (reduce +)))

(defn part2 []
  (->> input
       (partition 3)
       (map find-intersecting-item)
       (map (comp item->priority first))
       (reduce +)))

(comment
  (take 3 input)
  ;; => ("BccTFfTPTsffdDDqsq" "lGGLQwFhDgWdqvhW" "wbLNjGjlwLFrpSbllrHnHHRmmJVBmZJRRVcBTc")
  (part1)
  ;; => 7428
  (part2)
  ;; => 2650
  )
