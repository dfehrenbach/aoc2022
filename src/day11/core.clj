(ns day11.core
  (:require [clojure.string :as string]))

(defn parse-operation [s]
  (let [[_ op1 f op2] (re-find #"= (.+) ([+*]) (.+)" s)]
    (fn [old]
      ((eval (symbol f))
       (if (= op1 "old") old (parse-long op1))
       (if (= op2 "old") old (parse-long op2))))))

(defn extract-first-num [str]
  (parse-long (re-find #"\d+" str)))

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
    {:items (mapv parse-long (re-seq #"\d+" items-str))
     :op (parse-operation op-str)
     :test (parse-test test-str true-str false-str)
     :test-divisor (extract-first-num test-str)
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

(defn reduce-worry [item-val]
  (quot item-val 3))

(defn monkey-inspect [worry-fn all-monkeys monkey-n]
  (loop [monkeys all-monkeys]
    (let [monkey (get monkeys monkey-n)]
      (if-let [item (first (:items monkey))]
        (recur
         (-> monkeys
             (update-in [monkey-n :items] (comp vec rest))
             (update-in [monkey-n :items-inspected] inc)
             ((:test monkey) (worry-fn ((:op monkey) item)))))
        monkeys))))

(defn perform-round [worry-fn all-monkeys]
  (reduce (partial monkey-inspect worry-fn) all-monkeys (range 0 (count all-monkeys))))

(defn part-* [input worry-fn rounds]
  (->> (nth (iterate (partial perform-round worry-fn) input) rounds)
       (map :items-inspected)
       (sort >)
       (take 2)
       (reduce *)))

(defn part1 [input]
  (part-* input reduce-worry 20))

(defn worry-crt [divisor-product item-val]
  (rem item-val divisor-product))

(defn part2 [input]
  (let [divisor-product (reduce * (map :test-divisor input))]
    (part-* input (partial worry-crt divisor-product) 10000)))

(comment
  (take 1 input)
  ;; => ({:items #object[clojure.lang.PersistentQueue 0x6cf577c9 "clojure.lang.PersistentQueue@ed2"], 
  ;; :op #function[day11.core/parse-operation/fn--13276], 
  ;; :test #function[day11.core/parse-test/fn--13280], 
  ;; :test-divisor 17, 
  ;; :items-inspected 0})

  (time (part1 input))
  ;; => 120056

  (time (part2 test-input))
  ;; => 2713310158

  (time (part2 input))
  ;; => 21816744824
  0)
