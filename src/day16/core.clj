(ns day16.core
  (:require [clojure.string :as string]
            [loom.graph :as g]
            [loom.alg :as a]
            [loom.io :as io]))

;; Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
(defn parse-instruction [line]
  (let [[name & links] (re-seq #"[A-Z]{2}" line)
        flow-rate (parse-long (re-find #"\d+" line))]
    {:name name
     :links links
     :flow flow-rate}))

(defn parse-input [file]
  (->> (slurp file)
       string/split-lines
       (map parse-instruction)))

(def input
  (parse-input "src/day16/input.txt"))

(def test-input
  (parse-input "src/day16/test-input.txt"))

(defn create-graph [connections]
  (g/graph (reduce #(assoc %1 (:name %2) (:links %2)) {} connections)))

(defn name->flow [connections]
  (reduce #(assoc %1 (:name %2) (:flow %2)) {} connections))

(defn non-zero-flow-keys [connections]
  (map first (filter (comp pos? second) (name->flow connections))))

(defn find-valve-distances [input]
  (let [important-valves (conj (non-zero-flow-keys input) "AA")
        graph (create-graph input)]
    (for [start important-valves
          end (remove #{start} important-valves)]
      [start end (count (a/shortest-path graph start end))])))

(defn create-distance-graph [input]
  (let [all-dists-to-valves (find-valve-distances input)]
    (apply g/weighted-graph all-dists-to-valves)))

(defn dfs [graph start]
  (loop [node start
         visited #{start}
         time 30
         path []
         vented 0]
    (if (zero? time) {:path path :vented vented}
        (if-let [next-node (first (remove visited (g/successors graph node)))]
          (let [stuff 0]
            (recur next-node
                   (conj visited next-node)
                   time
                   path
                   vented))
          "nowhere else to go just set the time to 0. Everything should be added so far"))))


(comment
  (create-graph test-input)
  (name->flow test-input)

  (def test-input-graph (g/graph (create-graph test-input)))

  test-input-graph

  (io/dot-str test-input-graph)

  (a/shortest-path (g/graph (create-graph test-input)) "AA" "GG")

  ((a/all-pairs-shortest-paths (g/graph (create-graph test-input))) "AA")

  (non-zero-flow-keys test-input)

  (def dist-to-valve (for [start (conj (non-zero-flow-keys test-input) "AA")
                           end (remove #{start} (conj (non-zero-flow-keys test-input) "AA"))]
                       [start end (count (a/shortest-path test-input-graph start end))]))

  (def test-dist-graph (apply g/weighted-graph dist-to-valve))

  (defn penguin [g start remaining-time]
    (map (fn [s]
           (let [time' (- remaining-time
                          (g/weight g start s))]
             [s
              (* ((name->flow test-input) s) time')
              time'])) (g/successors g start)))

  (penguin test-dist-graph "AA" 30)
  ;; => (["JJ" 567 27] ["HH" 528 24] ["DD" 560 28] ["CC" 54 27] ["BB" 364 28] ["EE" 81 27]) ;;Here we actually chose DD over JJ for 560

  (penguin test-dist-graph "DD" 28)
  ;; => (["AA" 0 26] ["JJ" 504 24] ["HH" 506 23] ["CC" 52 26] ["BB" 325 25] ["EE" 78 26]) ;;Here we choose BB over HH or JJ for 325

  (penguin test-dist-graph "BB" 25)
  ;; => (["AA" 0 23] ["JJ" 441 21] ["HH" 396 18] ["DD" 440 22] ["CC" 46 23] ["EE" 63 21]) ;;Here we choose JJ for 441

  (penguin test-dist-graph "JJ" 21)
  ;; => (["AA" 0 18] ["HH" 286 13] ["DD" 340 17] ["CC" 32 16] ["BB" 221 17] ["EE" 48 16]) ;;Here we choose HH for 286

  (penguin test-dist-graph "HH" 13)
  ;; => (["AA" 0 7] ["JJ" 105 5] ["DD" 160 8] ["CC" 14 7] ["BB" 78 6] ["EE" 27 9]) ;;Here we choose EE for 27

  (penguin test-dist-graph "EE" 9)
  ;; => (["AA" 0 6] ["JJ" 84 4] ["HH" 110 5] ["DD" 140 7] ["CC" 12 6] ["BB" 65 5]) ;;Here we choose CC for 12

  (penguin test-dist-graph "CC" 6)
  ;; => (["AA" 0 3] ["JJ" 21 1] ["HH" 0 0] ["DD" 80 4] ["BB" 52 4] ["EE" 9 3]) 

  (+ 560 325 441 286 27 12)


  0)
