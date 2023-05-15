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

(defn next-data [graph flow-map time node node']
  (let [time' (- time (g/weight graph node node'))
        next-venting (* (flow-map node') time')]
    [time' next-venting]))

(defn dfs
  ([graph flow-map]
   (let [results (atom [])]
     (dfs graph flow-map "AA" 30 #{"AA"} 0 results)
     @results))
  ([graph flow-map node time visited vented results]
   (if (<= time 0) (swap! results conj vented)
       (if (first (remove visited (g/successors graph node)))
         (doseq [next-node (remove visited (g/successors graph node))]
           (let [[time' next-venting] (next-data graph flow-map time node next-node)]
             (dfs graph
                  flow-map
                  next-node
                  time'
                  (conj visited next-node)
                  (+ vented next-venting)
                  results)))
         (swap! results conj vented)))))

(defn part1 [input]
  (let [graph (create-distance-graph input)
        flow-map (name->flow input)]
    (apply max (dfs graph flow-map))))

(defn dfs-elephant
  ([graph flow-map]
   (let [results (atom [])]
     (dfs-elephant {:graph graph :flow-map flow-map :node "AA" :enode "AA" :time 26 :etime 26 :visited #{"AA"} :vented 0 :results results})
     @results))
  ([{:keys [graph flow-map node enode time etime visited vented results] :as data}]
   (if (and (<= etime 0) (<= time 0)) (swap! results conj vented)
       (if (first (remove visited (g/successors graph node)))
         (doseq [node' (remove visited (g/successors graph node))
                 enode' (remove (conj visited node') (g/successors graph enode))]
           (let [[time' next-venting] (next-data graph flow-map time node node')
                 [etime' next-eventing] (next-data graph flow-map etime enode enode')
                 vented' (+ vented
                            (if (<= time' 0) 0 next-venting)
                            (if (<= etime' 0) 0 next-eventing))
                 data' (-> data
                           (assoc :node node')
                           (assoc :enode enode')
                           (assoc :time time')
                           (assoc :etime etime')
                           (update :visited conj node' enode')
                           (assoc :vented vented'))]
             (dfs-elephant data')))
         (swap! results conj vented)))))

(defn part2 [input]
  (let [graph (create-distance-graph input)
        flow-map (name->flow input)]
    (apply max (dfs-elephant graph flow-map))))


(comment
  (part1 test-input)
  ;; => 1651

  (time (part1 input)) ;; 2110.09995791 msecs
  ;; => 1923


  (part2 test-input)
  ;; => 1707

  (part2 input)
  ;; => Execution error (OutOfMemoryError) at day16.core/dfs-elephant (core.clj:95).
  ;;    GC overhead limit exceeded

  ;; => 1707


  (count (dfs (create-distance-graph input) (name->flow input)))
  (count (dfs (create-distance-graph test-input) (name->flow test-input)))

  (count (dfs-elephant (create-distance-graph test-input) (name->flow test-input)))
  (apply max (dfs-elephant (create-distance-graph test-input) (name->flow test-input)))
  (create-distance-graph test-input)

  (+ 1 1)

  (apply max (dfs-elephant (create-distance-graph input) (name->flow input)))

  0)
