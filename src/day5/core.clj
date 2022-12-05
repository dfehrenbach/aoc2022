(ns day5.core
  (:require [clojure.string :as string]))

(def init-stacks
  (into (sorted-map)
        {1 '(\N \R \J \T \Z \B \D \F)
         2 '(\H \J \N \S \R)
         3 '(\Q \F \Z \G \J \N \R \C)
         4 '(\Q \T \R \G \N \V \F)
         5 '(\F \Q \T \L)
         6 '(\N \G \R \B \Z \W \C \Q)
         7 '(\M \H \N \S \L \C \F)
         8 '(\J \T \M \Q \N \D)
         9 '(\S \G \P)}))

(defn parse-instruction [line]
  (let [[_s move from to] (re-find #"move (\d+) from (\d+) to (\d+)" line)]
    {:move (parse-long move)
     :from (parse-long from)
     :to (parse-long to)}))

(def input
  (->> "src/day5/input.txt"
       slurp
       string/split-lines
       (map parse-instruction)))

(defn perform-move-p1 [{:keys [from to]} stacks]
  (->  stacks
       (update from pop)
       (update to conj (peek (stacks from)))))

(defn perform-instruction-p1 [stacks instruction]
  (nth (iterate (partial perform-move-p1 instruction) stacks)
       (:move instruction)))

(defn part1 []
  (->> input
       (reduce perform-instruction-p1 init-stacks)
       vals
       (map first)
       string/join))

(defn perform-instruction-p2 [stacks {:keys [move from to]}]
  (-> stacks
      (update from #(drop move %))
      (update to #(concat (take move (stacks from)) %))))

(defn part2 []
  (->> input
       (reduce perform-instruction-p2 init-stacks)
       vals
       (map first)
       string/join))

(comment
  (take 2 input)
  ;; => ({:move 3, :from 9, :to 4} {:move 2, :from 5, :to 2})
  (part1)
  ;; => "QNNTGTPFN"
  (part2)
  ;; => "GGNPJBTTR"
  )
