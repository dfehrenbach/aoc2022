(ns day15.core
  (:require [clojure.string :as string]))

(defn manhatten-dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

;;Sensor at x=3999724, y=2000469: closest beacon is at x=4281123, y=2282046
(defn parse-instruction [line]
  (let [[sx sy bx by] (map parse-long (re-seq #"[-]?\d+" line))]
    {:sensor [sx sy]
     :beacon [bx by]
     :manhatten-dist (manhatten-dist [sx sy] [bx by])
     :no-beacon-fn (fn [loc] (<= (manhatten-dist loc [sx sy])
                                 (manhatten-dist [sx sy] [bx by])))}))

(defn parse-input [file]
  (->> (slurp file)
       string/split-lines
       (map parse-instruction)))

(defn input []
  (parse-input "src/day15/input.txt"))

(defn test-input []
  (parse-input "src/day15/test-input.txt"))

#_(defn no-beacon? [input]
    (fn [loc] (map #(% loc) (map :no-beacon-fn input))))

#_(defn no-beacon-at-point? [input loc]
    (->> loc
         ((no-beacon? input))
         (some true?)))

(defn remaining-x-dist [b depth]
  (let [dist (- (:manhatten-dist b)
                (Math/abs (- (second (:sensor b)) depth)))]
    (when (nat-int? dist) dist)))

(defn ranges-at-depth [input depth]
  (remove nil? (map (fn [b]
                      (when-let [dist (remaining-x-dist b depth)]
                        [(- (first (:sensor b)) dist)
                         (+ (first (:sensor b)) dist)]))
                    input)))

(defn range-overlaps? [[start1 end1] [start2 end2]]
  (<= (max start1 start2)
      (min end1 end2)))

(defn combine-ranges
  "builds a set of distinct ranges with no overlaps"
  [ranges]
  (loop [ranges ranges
         finished []]
    (let [[a b & tail :as rs] ranges]
      (if (and a b)
        (if (range-overlaps? a b)
          (recur (concat [[(min (first a) (first b)) (max (second a) (second b))]] tail) finished)
          (recur (rest rs) (conj finished a)))
        (conj finished a b)))))

(defn part1 [input depth]
  (let [ranges (sort-by first (ranges-at-depth input depth))]
    (->> ranges
         combine-ranges
         (remove nil?)
         (map (fn [[left right]] (- right left)))
         (reduce +))))

(comment
  (first (input))

  (combine-ranges (sort-by first (ranges-at-depth (test-input) 10)))
  ;; => [[-1 5] [6 28] nil]

  (sort-by first (ranges-at-depth (test-input) 10))
  ;; => ([-2 2] [2 14] [14 18] [16 24])

  (sort-by first (ranges-at-depth (input) 2000000))
  ;; => ([-614713 2175035] [1080911 1762433] [1337953 2175035] [2045999 2175035] [2175035 2181349] [2175035 3945835] [2290375 2525663] [2862911 3170867] [3437217 4562231] [3471417 3999077])

  (part1 (test-input) 10)
  ;; => 26

  (part1 (input) 2000000)
  ;; => 5176944

  (sort-by :manhatten-dist > (input))
  ;; => ({:sensor [3060435 980430], :beacon [2175035 2000000], :manhatten-dist 1904970, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [1421672 3446889], :beacon [2408038 2645605], :manhatten-dist 1787650, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [780161 1907142], :beacon [2175035 2000000], :manhatten-dist 1487732, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3995530 8733], :beacon [3321979 692911], :manhatten-dist 1357729, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [168575 491461], :beacon [1053731 142061], :manhatten-dist 1234556, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [1884402 214904], :beacon [1053731 142061], :manhatten-dist 903514, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [53246 3908582], :beacon [152842 3117903], :manhatten-dist 890275, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3735247 2533767], :beacon [4281123 2282046], :manhatten-dist 797597, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3016889 2550239], :beacon [2408038 2645605], :manhatten-dist 704217, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3729564 3214899], :beacon [3610223 3768674], :manhatten-dist 673116, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3036853 3294727], :beacon [3191440 3801895], :manhatten-dist 661755, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3998355 3965954], :beacon [3610223 3768674], :manhatten-dist 585412, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3999724 2000469], :beacon [4281123 2282046], :manhatten-dist 562976, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [1756494 1928662], :beacon [2175035 2000000], :manhatten-dist 489879, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [206718 2732608], :beacon [152842 3117903], :manhatten-dist 439171, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [2820722 3865596], :beacon [3191440 3801895], :manhatten-dist 434419, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [2408019 2263990], :beacon [2408038 2645605], :manhatten-dist 381634, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3415633 3916020], :beacon [3191440 3801895], :manhatten-dist 338318, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3443945 3604888], :beacon [3610223 3768674], :manhatten-dist 330064, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [2110517 2243287], :beacon [2175035 2000000], :manhatten-dist 307805, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3704399 3973731], :beacon [3610223 3768674], :manhatten-dist 299233, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3889469 3781572], :beacon [3610223 3768674], :manhatten-dist 292144, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [2329102 2456329], :beacon [2408038 2645605], :manhatten-dist 268212, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3149491 3998374], :beacon [3191440 3801895], :manhatten-dist 238428, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [3256726 3882107], :beacon [3191440 3801895], :manhatten-dist 145498, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]} {:sensor [2178192 2132103], :beacon [2175035 2000000], :manhatten-dist 135260, :no-beacon-fn #function[day15.core/parse-instruction/fn--16899]})


  0)
