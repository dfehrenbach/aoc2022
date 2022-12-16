(ns day14.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn read-point [point-str]
  (mapv parse-long (string/split point-str #",")))

(defn build-line [[[x1 y1] [x2 y2]]]
  (letfn [(get-range [a b]
            (if (< a b)
              (range a (inc b))
              (range b (inc a))))]
    (cond
      (= x1 x2) (set (map #(vector x1 %) (get-range y1 y2)))
      (= y1 y2) (set (map #(vector % y1) (get-range x1 x2))))))

(defn build-lines [point-arr]
  (apply set/union (map build-line (partition 2 1 point-arr))))

(defn read-file [file]
  (->> (slurp file)
       string/split-lines
       (map #(string/split % #" -> "))
       (map #(map read-point %))
       (map build-lines)
       (apply set/union)))

(defn test-input []
  (read-file "src/day14/test-input.txt"))

(defn input []
  (read-file "src/day14/input.txt"))

(defn init-state [input] {:walls input
                          :abyss-depth (second (apply max-key second input))
                          :abyss false
                          :resting-sand #{}
                          :sand nil})
(defn sand-move-opts [[x y]]
  [[x (inc y)] ;;down
   [(dec x) (inc y)] ;;down-left
   [(inc x) (inc y)] ;;down-right
   ])

(defn sand-blocked? [resting-sand coord]
  (some #{coord} resting-sand))

(def wall-blocked?
  (memoize (fn [walls coord]
             (some #{coord} walls))))

(defn floor? [state [_ depth]]
  (= depth (+ 2 (:abyss-depth state))))

(defn next-sand-move [state]
  (first (drop-while (fn [coord] (or (wall-blocked? (:walls state) coord)
                                     (sand-blocked? (:resting-sand state) coord)
                                     (floor? state coord)))
                     (sand-move-opts (:sand state)))))

(defn step [state]
  (if-let [sand' (next-sand-move state)]
    (assoc state :sand sand')
    (-> state
        (assoc :sand nil)
        (update :resting-sand conj (:sand state)))))

(defn abyss? [state]
  (= (second (:sand state)) (:abyss-depth state)))

(defn perform-sand-falling [state]
  (cond
    (nil? (:sand state)) state
    (abyss? state) (assoc state :abyss true)
    :else (recur (step state))))

(defn new-sand [state]
  (assoc state :sand [500 0]))

(defn part1 [input]
  (->> (init-state input)
       (iterate (comp perform-sand-falling new-sand))
       (drop-while #(false? (:abyss %)))
       first :resting-sand count))

(defn search-next-gaps [state walls]
  (letfn [(sides [[x y]] [[(dec x) y] [(inc x) y]])]
    (for [w walls
          :let [[left right] (sides w)
                down [(first w) (inc (second w))]]
          :when (and (some #{left} walls)
                     (some #{right} walls)
                     (not (some #{down} walls))
                     (not (floor? state down)))]
      down)))

(defn search-all-gaps [state]
  (loop [next-walls (:walls state)]
    (let [next-gaps (search-next-gaps state next-walls)]
      (if (empty? next-gaps) next-walls
          (recur (set/union next-gaps next-walls))))))

(comment
  (search-all-gaps {:walls #{[500 0] [500 1] [500 2] [500 3]
                             [501 0] [502 0] [503 0]}}))

(defn triangle [floor-depth]
  (set (mapcat (fn [depth]
                 (map (fn [x] [x depth])
                      (range (- 500 depth) (inc (+ 500 depth)))))
               (range 0 floor-depth))))

(defn part2 [input]
  (let [state (init-state input)
        potential-sands (triangle (+ 2 (:abyss-depth state)))
        all-gaps-walls (search-all-gaps state)]
    (count (set/difference potential-sands all-gaps-walls))))

(comment
  (triangle (+ 2 (:abyss-depth (init-state (test-input))))))

(comment
  (take 5 (input))
  ;; => ([549 102] [510 29] [502 46] [525 148] [539 152])

  (part1 (test-input))
  ;; => 24

  (part1 (input))
  ;; => 1199

  (part2 (test-input))
  ;; => 93

  (part2 (input))
  ;; => 23925

  (count (:walls (init-state (test-input))))
  ;; => 20

  (count (search-all-gaps (init-state (test-input))))
  ;; => 37



  0)
