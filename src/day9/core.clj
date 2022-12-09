(ns day9.core
  (:require [clojure.string :as string]
            [clojure.math :as math]))

(defn parse-instruction [s]
  (let [[direction n] (string/split s #" ")]
    [direction (parse-long n)]))

(def input
  (->> "src/day9/input.txt"
       slurp
       string/split-lines
       (map parse-instruction)))

(defn init-state [knots] {:knots (repeat knots [0 0])
                          :tail-moves #{[0 0]}})

(defn move-in-direction [knot direction]
  (case direction
    "R" (update knot 0 inc)
    "L" (update knot 0 dec)
    "U" (update knot 1 inc)
    "D" (update knot 1 dec)))

(defn touching? [[hx hy] [tx ty]]
  (and (<= (Math/abs (- hx tx)) 1)
       (<= (Math/abs (- hy ty)) 1)))

(def move-tail
  (memoize
   (fn [[hx hy :as h] [tx ty :as t]]
     (let [dx (- hx tx)
           dy (- hy ty)]
       (if (touching? h t) t
           [(+ tx  (math/signum dx))
            (+ ty  (math/signum dy))])))))

(defn move-rope [[h & knots] direction]
  (reductions move-tail (move-in-direction h direction) knots))

(defn step [direction state]
  (let [knots' (move-rope (:knots state) direction)]
    (-> state
        (assoc :knots knots')
        (update :tail-moves conj (last knots')))))

(defn perform-instruction [state [direction move-n]]
  (nth (iterate (partial step direction) state)
       move-n))

(defn part-x [knots]
  (->> input
       (reduce perform-instruction (init-state knots))
       :tail-moves
       count))

(defn part1 []
  (part-x 2))

(defn part2 []
  (part-x 10))

(comment
  (take 5 input)
  ;; => (["R" 1] ["D" 1] ["U" 1] ["R" 2] ["L" 2])

  (time (part1))
  ;; => 5902
  ;; Elapsed time: 28ms
  ;; Elapsed time: 27ms

  (time (part2))
  ;; => 2445
  ;; Elapsed time: 562ms
  ;; Elapsed time: 147ms
  ;; Elapsed time: 143ms
  )
