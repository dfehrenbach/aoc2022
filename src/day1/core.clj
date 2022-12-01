(ns day1.core
  (:require [clojure.string :as string]))

(defn build-groups [calories-list]
  (loop [calories-groups []
         current-list calories-list]
    (if (empty? current-list) calories-groups
        (let [[next-group remaining-list] (split-with (partial not= "") current-list)]
          (recur (conj calories-groups (map read-string next-group))
                 (rest remaining-list))))))

(def input
  (->> "src/day1/input.txt"
       slurp
       string/split-lines
       build-groups))

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
  (part1)
  ;; 68775
  (part2)
  ;; 202585
  )
