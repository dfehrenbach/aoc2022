(ns day10.core
  (:require [clojure.string :as string]))

(defn parse-cmd [line]
  (let [[cmd & args] (string/split line #" ")]
    (case cmd
      "noop" [:noop args]
      "addx" [:addx (first (map parse-long args))])))

(def input
  (->> "src/day10/input.txt"
       slurp
       string/split-lines
       (map parse-cmd)))

(def test-input
  (->> "src/day10/test-input.txt"
       slurp
       string/split-lines
       (map parse-cmd)))

(defn noop [_arg state]
  (identity state))

(defn addx [arg state]
  (update state :x + arg))

(def operation->fn {:noop [noop]
                    :addx [noop, addx]})

(defn apply-args [[operation arg]]
  (map #(partial % arg) (operation->fn operation)))

(defn signal-strength [all-states n]
  (* (:x (nth all-states (dec n)))
     n))

(defn get-all-states [input]
  (->> input
       (mapcat apply-args)
       (reductions (fn [state f] (f state)) {:x 1})))

(defn part1 [input]
  (->> [20 60 100 140 180 220]
       (map (partial signal-strength (get-all-states input)))
       (reduce +)))

(defn build-line [forty-states]
  (map (fn [{:keys [x]} pixel-n]
         (if (some #(= % pixel-n) [(dec x) x (inc x)])
           "#" "."))
       forty-states
       (range 0 40)))

(defn part2 [input]
  (->> input
       get-all-states
       (partition 40)
       (map build-line)
       (map string/join)
       (map prn)))

(comment
  (take 5 input)
  ;; => ([:noop nil] [:noop nil] [:noop nil] [:addx 6] [:addx -1])

  (part1 test-input)
  ;; => 13140
  (part1 input)
  ;; => 14340

  (part2 test-input)
  ;; => (nil nil nil nil nil nil)
  ;; "##..##..##..##..##..##..##..##..##..##.."
  ;; "###...###...###...###...###...###...###."
  ;; "####....####....####....####....####...."
  ;; "#####.....#####.....#####.....#####....."
  ;; "######......######......######......####"
  ;; "#######.......#######.......#######....."
  (part2 input)
  ;; => (nil nil nil nil nil nil)
  ;; "###...##..###....##..##..###..#..#.###.."
  ;; "#..#.#..#.#..#....#.#..#.#..#.#..#.#..#."
  ;; "#..#.#..#.#..#....#.#....###..####.#..#."
  ;; "###..####.###.....#.#....#..#.#..#.###.."
  ;; "#....#..#.#....#..#.#..#.#..#.#..#.#...."
  ;; "#....#..#.#.....##...##..###..#..#.#....

  0)
