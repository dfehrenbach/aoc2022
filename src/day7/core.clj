(ns day7.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day7/input.txt"
       slurp
       string/split-lines))

(def test-input
  (->> "src/day7/test-input.txt"
       slurp
       string/split-lines))

(def init-system {"/" {:size 0 :children {} :type :dir}})

;; keep track of dir path
;; [/ a b c] as /a/b/c and uncover file sizes
;; can either build as you go by taking all discovered files and adding to total on each path
;; or build numbers at the end doing one level at a time from the bottom up
(defn init-dir [system path instruction]
  (let [name (second (string/split instruction #" "))]
    (assoc-in system (concat (interleave path (repeat :children)) [name])
              {:size 0 :children {} :type :dir})))

(defn init-file [system path size name]
  (assoc-in system (concat (interleave path (repeat :children)) [name])
            {:size size :type :file}))

(defn cascade-file-size-upwards [system path size]
  (let [all-paths (rest (reductions conj [] path))]
    (reduce (fn [acc-system this-path]
              (update-in acc-system
                         (concat (butlast (interleave this-path (repeat :children))) [:size])
                         + size))
            system all-paths)))

(defn parse-cd-path [instruction path]
  (let [name (nth (string/split instruction #" ") 2)]
    (case name
      "/" ["/"]
      ".." (pop path)
      (conj path name))))

(defn parse-new-file [system path instruction]
  (let [[size-str name] (string/split instruction #" ")]
    (-> system
        (init-file path (parse-long size-str) name)
        (cascade-file-size-upwards path (parse-long size-str)))))

(defn read-instructions [input]
  (loop [instructions input
         system init-system
         path []]
    (if (empty? instructions) system
        (let [[instruction & rest-instructions] instructions
              [next-instructions next-system next-path]
              (cond
                (string/starts-with? instruction "$ cd") [rest-instructions system (parse-cd-path instruction path)]
                (string/starts-with? instruction "$ ls") [rest-instructions system path]
                (string/starts-with? instruction "dir") [rest-instructions (init-dir system path instruction) path]
                (re-matches #"\d.*" instruction) [rest-instructions (parse-new-file system path instruction) path])]
          (recur next-instructions next-system next-path)))))

;;Then it's about walking the tree to find sizes that dip under


(comment
  (take 5 input)

  #_(tree-seq (fn [node]
                (prn node)
                (:children (first (vals node)))) (comp :children first vals)
              {"/" {:children {:a 1 :b 2}}})

  #_(count (tree-seq #(= :dir (:type (first (vals %)))) (comp :children first vals)
                     (read-instructions test-input)))

  ;; test-input results
  {"/" {:size 48381165, :children
        {"a" {:size 94853, :children
              {"e" {:size 584, :children
                    {"i" {:size 584, :type :file}},
                    :type :dir},
               "f" {:size 29116, :type :file},
               "g" {:size 2557, :type :file},
               "h.lst" {:size 62596, :type :file}}, :type :dir},
         "b.txt" {:size 14848514, :type :file},
         "c.dat" {:size 8504156, :type :file},
         "d" {:size 24933642, :children
              {"j" {:size 4060174, :type :file},
               "d.log" {:size 8033020, :type :file},
               "d.ext" {:size 5626152, :type :file},
               "k" {:size 7214296, :type :file}},
              :type :dir}},
        :type :dir}}

  (read-instructions input)
  0)
