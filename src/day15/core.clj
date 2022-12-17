(ns day15.core
  (:require [clojure.string :as string]))

;; Retro
;; So, this one was tricky, and I ended up with a slick-ish math answer to discovering overlaps with ranges, sorting the ranges by lower bound, and combining them
;; I ended up with a function I could run for every row that would give me the distinc ranges of numbers "covered" by the "sensor can't be here" logic
;; Alas, I as stuck with a function that I had to run up to 4 million times (upper y bound)
;; Additionally, I got lucky with not being "inclusive" on the right, subtracting out the single sensor that happened to be on that row by adding exclusively.
;; A better solve to the problem, at the end, would have been to describing each sensor's outerbounds by 4 line segments.
;; We know that only a SINGLE space wasn't going to be covered by the sensor, so the answer would fall on ONE of these line segments.
;; So, looking for the intersections, you might find sensors that agree that an answer could be there.
;; Finally, you put all of these under scrutiny looking for any that have exist past all manhatten dists (or has 4 intersections) and voila
;; Most of my struggles with the second part still exist in the bottom comment block beneath the supposed answer. Woof. What a problem.
;; Simplist solution might even be to take away the line-segment restriction
;; There's more intersections, but it's certainly only at max (* 2 (count input)) per line which you have (* 4 (count input)). 
;; Easy to check a couple hundred intersecting lines without turning them into line segments.

(defn manhatten-dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

;;Sensor at x=3999724, y=2000469: closest beacon is at x=4281123, y=2282046
(defn parse-instruction [line]
  (let [[sx sy bx by] (map parse-long (re-seq #"[-]?\d+" line))]
    {:sensor [sx sy]
     :beacon [bx by]
     :manhatten-dist (manhatten-dist [sx sy] [bx by])}))

(defn parse-input [file]
  (->> (slurp file)
       string/split-lines
       (map parse-instruction)))

(defn input []
  (parse-input "src/day15/input.txt"))

(defn test-input []
  (parse-input "src/day15/test-input.txt"))

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
  (<= (dec (max start1 start2))
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

(defn part2a
  "Find the first range on a line where there is a gap in the ranges. 
   Should provide x coordinate and y coordinate via depth.
   Unfortunately, very slow and requires up to 4,000,000 scans.
   Answer was found but left for some unknown minutes as I went to bed"
  [input y-bound]
  (let [[x y] (first (drop-while #(= 2 (count (first %)))
                                 (map (fn [depth] [(combine-ranges (sort-by first (ranges-at-depth input depth))) depth])
                                      (range 0 (inc y-bound)))))]
    [(inc (second (first x))) y]))

(defn part2b [sol2a]
  (+ (* (first sol2a) 4000000) (second sol2a)))

(comment
  (first (input))

  (sort-by first (ranges-at-depth (test-input) 10))
  ;; => ([-2 2] [2 14] [14 18] [16 24])

  (sort-by first (ranges-at-depth (input) 2000000))
  ;; => ([-614713 2175035] [1080911 1762433] [1337953 2175035] [2045999 2175035] [2175035 2181349] [2175035 3945835] [2290375 2525663] [2862911 3170867] [3437217 4562231] [3471417 3999077])

  (part1 (test-input) 10)
  ;; => 26

  (part1 (input) 2000000)
  ;; => 5176944

  (part2a (test-input) 20)
  ;; => [14 11]

  (part2b (part2a (test-input) 20))
  ;; => 56000011

  (part2a (input) 4000000)
  ;; => [3337614 2933732]

  (part2b [3337614 2933732])
  ;; => 13350458933732


  (first (drop-while #(= 2 (count %))
                     (map (fn [depth] (combine-ranges (sort-by first (ranges-at-depth (input) depth))))
                          (range 0 4000000))))
  ;; => [[-337013 3337613] [3337615 4132879] nil]
  ;; 3337614


  (defn remaining-y-width [b width]
    (let [dist (- (:manhatten-dist b)
                  (Math/abs (- (first (:sensor b)) width)))]
      (when (nat-int? dist) dist)))

  (defn ranges-at-width [input width]
    (remove nil? (map (fn [b]
                        (when-let [dist (remaining-y-width b width)]
                          [(- (second (:sensor b)) dist)
                           (+ (second (:sensor b)) dist)]))
                      input)))

  (combine-ranges (sort-by first (ranges-at-width (input) 3337614)))
  ;; => [[-708546 2933731] [2933733 4176319] nil]
  ;; => 2933732

  (combine-ranges (sort-by first (ranges-at-depth (input) 2933732)))
  ;; => [[-337013 3337613] [3337615 4132879] nil]

  (+ (* 3337614 4000000) 2933732)
  ;; => 13350458933732




  0)
