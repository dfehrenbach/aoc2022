(ns day11.core
  (:require [clojure.string :as string]))

(defn parse-operation [s]
  (let [[_ op1 f op2] (re-find #"= (.+) ([+*]) (.+)" s)]
    (fn [old]
      ((eval (symbol f))
       (if (= op1 "old") old (bigint (parse-long op1)))
       (if (= op2 "old") old (bigint (parse-long op2)))))))

(defn extract-first-num [str]
  (bigint (parse-long (re-find #"\d+" str))))

(defn parse-test [test-str true-str false-str]
  (let [div-by (extract-first-num test-str)
        true-monkey (extract-first-num true-str)
        false-monkey (extract-first-num false-str)]
    (fn [all-monkeys item-value]
      (if (zero? (mod item-value div-by))
        (update-in all-monkeys [true-monkey :items] conj item-value)
        (update-in all-monkeys [false-monkey :items] conj item-value)))))

(defn parse-monkey [lines]
  (let [[_ items-str op-str test-str true-str false-str] lines]
    {:items (mapv (comp bigint parse-long) (re-seq #"\d+" items-str))
     :op (parse-operation op-str)
     :test (parse-test test-str true-str false-str)
     :items-inspected 0}))

(def input
  (->> "src/day11/input.txt"
       slurp
       string/split-lines
       (partition-by #(= % ""))
       (remove #{'("")})
       (mapv parse-monkey)))

(def test-input
  (->> "src/day11/test-input.txt"
       slurp
       string/split-lines
       (partition-by #(= % ""))
       (remove #{'("")})
       (mapv parse-monkey)))


(defn monkey-inspect [worry-div all-monkeys monkey-n]
  (let [monkey (get all-monkeys monkey-n)]
    (if-let [item (first (:items monkey))]
      (recur worry-div
             (-> all-monkeys
                 (update-in [monkey-n :items] (comp vec rest))
                 (update-in [monkey-n :items-inspected] inc)
                 ((:test monkey) (quot ((:op monkey) item) worry-div)))
             monkey-n)
      all-monkeys)))

(defn perform-round [worry-div all-monkeys]
  (reduce (partial monkey-inspect worry-div) all-monkeys (range 0 (count all-monkeys))))

(defn part-* [input worry-div rounds]
  (->> (nth (iterate (partial perform-round worry-div) input) rounds)
       (map :items-inspected)
       (sort >)
       (take 2)
       (reduce *)))

(defn part1 [input]
  (part-* input 3 20))

(defn monkey-inspect2 [all-monkeys monkey-n]
  (let [monkey (get all-monkeys monkey-n)]
    (if-let [item (first (:items monkey))]
      (recur (-> all-monkeys
                 (update-in [monkey-n :items] (comp vec rest))
                 (update-in [monkey-n :items-inspected] inc)
                 ((:test monkey) ((:op monkey) item)))
             monkey-n)
      all-monkeys)))

(defn perform-round2 [all-monkeys]
  (reduce monkey-inspect2 all-monkeys (range 0 (count all-monkeys))))

(defn part-*2 [input rounds]
  (->> (nth (iterate perform-round2 input) rounds)
       (map :items-inspected)
       (sort >)
       (take 2)
       (reduce *)))

(defn part2 [input]
  (part-*2 input 100))


(comment
  (count input)

  (part1 input)
  ;; => 120056

  ;; woof.
  ;; 10,000 rounds
  ;; * 8 monkeys = 80,000 monkey turns
  ;; and then there's 0 to many inspections per turn.
  ;; the test input ended up with 150k+ 
  ;; every function has crazy load, (< round, turn, inspect)



  0)
