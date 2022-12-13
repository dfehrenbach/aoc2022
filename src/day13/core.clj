(ns day13.core
  (:require [clojure.string :as string]))

(defn read-file [file]
  (->> (slurp file)
       string/split-lines
       (partition-by #(= % ""))
       (remove #{'("")})
       (map #(map read-string %))))

(defn test-input []
  (read-file "src/day13/test-input.txt"))

(defn input []
  (read-file "src/day13/input.txt"))

(defn cmp [left right]
  (cond
    (or (nil? left) (nil? right))
    (compare left right)

    (and (seqable? left) (seqable? right))
    (let [first-result (cmp (first left) (first right))]
      (if (zero? first-result) (cmp (seq (rest left)) (seq (rest right)))
          first-result))

    (and (int? left) (int? right))
    (compare left right)

    (and (seqable? left) (int? right))
    (cmp left [right])

    (and (int? left) (seqable? right))
    (cmp [left] right)))

(defn part1 [input]
  (->> input
       (map #(cmp (first %) (second %)))
       (keep-indexed (fn [idx result] (when (= result -1) (inc idx))))
       (reduce +)))

(defn part2 [input]
  (->> input
       (apply concat)
       (#(conj % [[2]] [[6]]))
       (sort cmp)
       (keep-indexed (fn [idx packet] (when (or (= packet [[2]]) (= packet [[6]])) (inc idx))))
       (reduce *)))

(comment
  (take 1 (test-input))
  ;; => (([1 1 3 1 1] [1 1 5 1 1]))
  (part1 (test-input))
  ;; => 13
  (part1 (input))
  ;; => 5882
  (part2 (test-input))
  ;; => 140
  (part2 (input))
  ;; => 24948
  )
