(ns day6.core)

(def input
  (->> "src/day6/input.txt"
       slurp))

(defn find-bad-windows [window input]
  (->> input
       (partition window 1 input)
       (map (comp count set))
       (take-while (partial not= window))))

(defn part1 [] ;; window of 4
  (->> input
       (find-bad-windows 4)
       count
       (+ 4)))

(defn part2 [] ;; window of 14
  (->> input
       (find-bad-windows 14)
       count
       (+ 14)))

(comment
  (take 5 input)
  ;; => (\r \g \f \f \b)
  (time (part1))
  ;; => 1100
  (time (part2))
  ;; => 2421
  )
