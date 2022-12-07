(ns day7.core
  (:require [clojure.string :as string]))

(def input
  (->> "src/day7/input.txt"
       slurp
       string/split-lines))

(def init-system {"/" {:size 0 :children {} :type :dir}})

;; keep track of dir path
;; [/ a b c] as /a/b/c and uncover file sizes
;; Going with -> THIS ONE: can either build as you go by taking all discovered files and adding to total on each path
;; or build numbers at the end doing one level at a time from the bottom up

(defn init-dir [system path instruction]
  (let [name (second (string/split instruction #" "))]
    (assoc-in system (concat (interleave path (repeat :children)) [name])
              {:size 0 :children {} :type :dir :name name})))

(defn init-file [system path size name]
  (assoc-in system (concat (interleave path (repeat :children)) [name])
            {:size size :type :file :name name}))

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

(defn add-in-new-file [system path instruction]
  (let [[size-str name] (string/split instruction #" ")]
    (-> system
        (init-file path (parse-long size-str) name)
        (cascade-file-size-upwards path (parse-long size-str)))))

(defn read-instructions [input]
  (loop [instructions input
         system init-system
         path []]
    (if (empty? instructions) system
        (let [[instruction & rest-instructions] instructions]
          (cond
            (string/starts-with? instruction "$ cd")
            (recur rest-instructions system (parse-cd-path instruction path))

            (string/starts-with? instruction "$ ls")
            (recur rest-instructions system path)

            (string/starts-with? instruction "dir")
            (recur rest-instructions (init-dir system path instruction) path)

            (re-matches #"\d.*" instruction)
            (recur rest-instructions (add-in-new-file system path instruction) path))))))

;;Then it's about walking the tree to find directory sizes that dip under
(defn all-directories [system]
  (filter #(and (map? %)
                (contains? % :type)
                (= :dir (:type %)))
          (tree-seq map? vals system)))

(defn part1 [input]
  (->> input
       read-instructions
       all-directories
       (map :size)
       (filter (partial >= 100000))
       (reduce +)))

(defn part2 [input]
  (let [full-system (read-instructions input)
        used-space (get-in full-system ["/" :size])]
    (->> full-system
         all-directories
         (map :size)
         (filter #(<= 30000000 (+ % (- 70000000 used-space))))
         (apply min))))

(comment
  (take 5 input)
  ;; => ("$ cd /" "$ ls" "dir ddpgzpc" "dir mqjrd" "dir mrqjg")

  (nth (->> input read-instructions all-directories) 5)
  ;; => {:size 74360, :children {"zjq" {:size 74360, :type :file, :name "zjq"}}, 
  ;;     :type :dir, :name "dtgnbb"}

  (part1 input)
  ;; => 1915606

  (part2 input)
  ;; => 5025657
  )
