(ns day12.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day12/input.txt"
       slurp
       string/split-lines
       (map #(string/split % #""))))

(def test-input
  (->> "src/day12/test-input.txt"
       slurp
       string/split-lines
       (map #(string/split % #""))))

(defn find-coords-for-val [m s]
  (apply concat (remove empty?
                        (keep-indexed
                         (fn [yidx vs]
                           (keep-indexed
                            (fn [xidx v] (when (= v s) [yidx xidx])) vs))
                         m))))

(defn find-start [m]
  (first (find-coords-for-val m "S")))
(defn find-end [m]
  (first (find-coords-for-val m "E")))

(defn climbable? [start-val end-val]
  (<= end-val (inc start-val)))

(defn possible-moves [m visited [y x :as coord]]
  (let [elevation (get-in m coord 99)]
    (filter (fn [dest-coord]
              (and (not (some #{dest-coord} visited))
                   (climbable? elevation (get-in m dest-coord 99))))
            [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]])))

(defn char->elevation [s]
  (case s
    "S" 0
    "E" 25
    (- (int (first (seq s))) 97)))

(defn init-state [input] {:start (find-start input)
                          :end (find-end input)
                          :matrix (mapv #(mapv char->elevation %) input)
                          :visited #{(find-start input)}
                          :layers []})

(defn bfs [state starts ends]
  (if (empty? starts) :fail
      (if (some ends starts) state
          (let [next-layer' (set (mapcat
                                  #(possible-moves (:matrix state) (:visited state) %)
                                  starts))]
            (recur (-> state
                       (update :visited #(apply (partial conj %) next-layer'))
                       (update :layers conj next-layer'))
                   next-layer' ends)))))

(defn part1 [input]
  (let [state (init-state input)]
    (->> (bfs state #{(:start state)} #{(:end state)})
         :layers
         count)))

(defn invert-matrix [m]
  (mapv (fn [row] (mapv #(Math/abs (- 25 %)) row)) m))

(defn part2 [input]
  (let [state (update (init-state input) :matrix invert-matrix)]
    (->> (bfs state #{(:end state)} (set (find-coords-for-val input "a")))
         :layers
         count)))

(comment
  (take 1 input)
  ;; => (["a" "b" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "c" "c" "a" "a" "a" "a" "a" "a" "a" "a" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "a" "a" "a" "a" "a" "a"])
  (find-start input)
  ;; => [20 0]
  (find-end input)
  ;; => [20 77]
  (part1 test-input)
  ;; => 31
  (part1 input)
  ;; => 361
  (part2 test-input)
  ;; => 29
  (part2 input)
  ;; => 354
  0)
