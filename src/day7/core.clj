(ns day7.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day7/input.txt"
       slurp
       string/split-lines))

(defn cascade-sizes [system path size]
  (let [all-paths (rest (reductions conj [] path))]
    (reduce (fn [sys p]
              (let [current-size (get-in sys (conj p :size) 0)]
                (assoc-in sys (conj p :size) (+ (parse-long size) current-size))))
            system all-paths)))

(defn read-instructions3 [input]
  (loop [cmds input
         system {}
         path []]
    (if (empty? cmds) system
        (let [cmd (first cmds)]
          (cond
            (string/starts-with? cmd "$ ls")
            (recur (rest cmds) system path)

            (string/starts-with? cmd "$ cd /")
            (recur (rest cmds) system ["/"])

            (string/starts-with? cmd "$ cd ..")
            (recur (rest cmds) system (pop path))

            (string/starts-with? cmd "$ cd")
            (recur (rest cmds) system (conj path (nth (string/split cmd #" ") 2)))

            (string/starts-with? cmd "dir")
            (recur (rest cmds) system path)

            (re-matches #"(\d+) (.*)" cmd)
            (let [[_ size name] (re-matches #"(\d+) (.*)" cmd)]
              (recur (rest cmds)
                     (-> system
                         (update-in path assoc name (parse-long size))
                         (cascade-sizes path size))
                     path)))))))

(defn all-directory-sizes [system]
  (->> system
       (tree-seq map? vals)
       (map :size)
       (remove nil?)))

(defn part11 [input]
  (->> input
       read-instructions3
       all-directory-sizes
       (filter (partial >= 100000))
       (reduce +)))

(defn part22 [input]
  (let [full-system (read-instructions3 input)
        used-space (get-in full-system ["/" :size])]
    (->> full-system
         all-directory-sizes
         (filter #(<= (- used-space (- 70000000 30000000)) %))
         (apply min))))


(comment
  (take 5 input)
  ;; => ("$ cd /" "$ ls" "dir ddpgzpc" "dir mqjrd" "dir mrqjg")

  (part11 input)
  ;; => 1915606

  (part22 input)
  ;; => 5025657

  0)
