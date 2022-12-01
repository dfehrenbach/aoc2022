(ns day1.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day1/input.txt"
       slurp
       string/split-lines
       (map parse-long)
       (partition-by some?)
       (remove #{'(nil)})))

(defn part1 []
  (->> input
       (map #(reduce + %))
       (apply max)))

(defn part2 []
  (->> input
       (map #(reduce + %))
       (sort >)
       (take 3)
       (reduce +)))

(comment
  (take 1 input)
  ;; => ((3427 3273 5615 5943 3125 4245 4194 3243 4283 1790 5355 4239 5541))
  (part1)
  ;; => 68775
  (part2)
  ;; => 202585
  )
