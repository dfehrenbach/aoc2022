(ns day8.core
  (:require [clojure.string :as string]))

(defn parse-tree [s]
  {:visible false :height (parse-long s)})

(defn injest-input [file]
  (->> file
       slurp
       string/split-lines
       (map #(string/split % #""))
       (mapv #(mapv parse-tree %))))

(def input (injest-input "src/day8/input.txt"))
(def test-input (injest-input "src/day8/test-input.txt"))


(defn transpose [forrest-matrix]
  (apply mapv vector forrest-matrix))

(defn mark-visible-trees [line]
  (loop [i 0
         marked-line line
         tallest -1]
    (if-let [tree (nth line i nil)]
      (recur (inc i)
             (if (< tallest (:height tree)) (assoc-in marked-line [i :visible] true) marked-line)
             (if (< tallest (:height tree)) (:height tree) tallest))
      marked-line)))

(defn count-visible-trees [forrest-matrix]
  (->> forrest-matrix
       (map #(filter :visible %))
       (map count)
       (reduce +)))

(defn part1 [input]
  (->> input
       (mapv mark-visible-trees) ;; from-left
       (mapv (comp vec reverse))
       (mapv mark-visible-trees) ;; from-right
       transpose
       (mapv mark-visible-trees) ;; from-top 
       (mapv (comp vec reverse))
       (mapv mark-visible-trees) ;; from-bottom 
       count-visible-trees))

(defn visible-trees-to-right-of [line loc]
  (let [[tree-house & trees] (map :height (drop loc line))
        [short tall] (split-with #(< % tree-house) trees)]
    (count (remove nil? (conj short (first tall))))))

(defn mark-view [k line]
  (loop [i 0
         marked-line line]
    (if (nth line i nil)
      (recur (inc i)
             (assoc-in marked-line [i k] (visible-trees-to-right-of marked-line i)))
      marked-line)))

(defn calc-scenery-score [{:keys [right left down up] :as _tree}]
  (* right left down up))

(defn part2 [input]
  (->> input
       (mapv (partial mark-view :right))
       (mapv (comp vec reverse))
       (mapv (partial mark-view :left))
       transpose
       (mapv (partial mark-view :down))
       (mapv (comp vec reverse))
       (mapv (partial mark-view :up))
       (mapcat #(map calc-scenery-score %))
       (apply max)))


(comment
  (take 3 (first input))
  ;; => ({:visible false, :height 2} {:visible false, :height 0} {:visible false, :height 0})
  (part1 test-input)
  ;; => 21
  (part1 input)
  ;; => 1818
  (part2 test-input)
  ;; => 8
  (part2 input)
  ;; => 368368
  )
