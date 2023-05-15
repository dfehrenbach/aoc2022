(ns aspects.core
  (:require [loom.graph :as g]
            [loom.io :as io]
            [clojure.data.json :as json]))

(def druid-codex
  [[:wolf :nature :storm :crit :shapeshift]
   [:companion]
   [:lucky :nature :storm]
   [:wolf :crit :shapeshift]
   [:earth :cc :nature]
   [:nature :storm :hurricane]
   [:earth :nature :pulverize :bear :shapeshift]
   [:bear :crit :critdamage :shapeshift]
   [:fortify :earth :nature]
   [:earth :nature :bulwark]
   [:shapeshift :fortify]
   [:shapeshift :wolf]
   [:nature :storm :cyclonearmor]
   [:earth :nature :slow :storm]
   [:earth :nature :fortify]
   [:shapeshift :hurricane]
   [:windshear :spirit :storm :nature]
   [:poison :bear :wolf :shapeshift]
   [:spirit :cc]
   [:wolf :spirit]
   [:bear :spirit]])

(def druid-non-codex
  [[:nature :earth :boulder]
   [:storm :nature :earth]
   [:bear :nature :earth :storm :wolf]
   [:nature :earth :landslide]
   [:wolf :shapeshift :companion :poison]
   [:wolf :shapeshift :shred :poison]
   [:companion]
   [:nature :earth :landslide :bear :trample]
   [:companions :wolf :bear :shapeshift]
   [:wolf :shapeshift :spirit]
   [:storm :nature :lightningstorm]
   [:storm :nature :crit]
   [:storm :nature :crit]
   [:earth :earthspike :spirit]
   [:bear :pulverize :shapeshift]
   [:storm :tornado :nature]
   [:earth :nature :bulwark :cc]
   [:bear :shapeshift :roar :poison :cc]
   [:earth :storm :nature]
   [:spirit :bear :shapeshift]])

(def druid-all
  (concat druid-codex druid-non-codex))

(defn make-map [base as]
  (reduce
   (fn [m a]
     (if (contains? m a)
       (update m a conj as)
       (assoc m a [as])))
   base as))

(defn group-by-tag [aspects]
  (reduce make-map {} aspects))

(defn make-tag-relationships [aspects]
  (let [tag->aspect (group-by-tag aspects)]
    (zipmap
     (keys tag->aspect)
     (map #(frequencies (flatten %)) (vals tag->aspect)))))

(defn remove-key-from-relationships [relationships]
  (reduce
   (fn [m [k v]] (assoc m k (dissoc v k)))
   {} relationships))

(defn rgb->cymk [{r :r g :g b :b}]
  (if (and (= r 0) (= g 0) (= b 0))
    {:c 0, :y 0, :m 0, :k 1}
    (let [c (- 1 (/ r 255))
          m (- 1 (/ g 255))
          y (- 1 (/ b 255))
          min-cmy (apply min [c m y])
          c' (/ (- c min-cmy) (- 1 min-cmy))
          m' (/ (- m min-cmy) (- 1 min-cmy))
          y' (/ (- y min-cmy) (- 1 min-cmy))]
      {:c c', :m m', :y y', :k min-cmy})))

(defn cymk->rgb [{c :c y :y m :m k :k}]
  (let [r (* 255 (- 1 c) (- 1 k))
        g (* 255 (- 1 m) (- 1 k))
        b (* 255 (- 1 y) (- 1 k))]
    {:r (int r) :g (int g) :b (int b)}))

(defn mix-cymk [cymk1 cymk2]
  (let [{c1 :c y1 :y m1 :m k1 :k} cymk1
        {c2 :c y2 :y m2 :m k2 :k} cymk2]
    {:c (/ (+ c1 c2) 2) :y (/ (+ y1 y2) 2) :m (/ (+ m1 m2) 2) :k (/ (+ k1 k2) 2)}))

(defn color-mix [rgb1 rgb2]
  (->> [rgb1 rgb2]
       (map rgb->cymk)
       (apply mix-cymk)
       cymk->rgb))

(def tag->color
  {:earth {:r 164 :g 209 :b 148}
   :storm {:r 165, :g 227, :b 243}
   :wolf {:r 34, :g 100, :b 119}
   :bear {:r 142 :g 63 :b 11}})

(defn derive-color [relationship]
  (let [mixable-colors (->> relationship
                            (filter #(contains? tag->color (key %)))
                            (mapcat (fn [[k v]] (repeat v (tag->color k)))))]
    (case (vec mixable-colors)
      [] {:r 0 :g 0 :b 0}
      [c1] (first mixable-colors)
      (reduce color-mix mixable-colors))))

(defn convert-color [color]
  (let [{r :r g :g b :b} color]
    (format "#%02x%02x%02x" r g b)))

(defn get-links [relationships]
  (vec (mapcat
        (fn [[k v]]
          (mapv
           (fn [[k2 v2]]
             {:source k :target k2 :val v2})
           v))
        relationships)))

(defn get-json [aspects]
  (let [relationships (make-tag-relationships aspects)
        attributes-with-color (mapv
                               (fn [[k v]]
                                 {:id k :name k :val (count v)
                                  :color (convert-color (or (tag->color k) (derive-color (relationships k))))})
                               (group-by-tag aspects))]
    (json/write-str
     {:nodes attributes-with-color
      :links (get-links (remove-key-from-relationships relationships))})))

(comment
  ;; test for make-map
  (make-map {} [:cc :spirit :wolf])

  ;; what does a single item look like?
  (:earth (group-by-tag druid-codex))

  ;; data relationships test (inner map for values)
  (frequencies (flatten (:earth (group-by-tag druid-codex))))

  ;; data relationships
  (zipmap (keys (group-by-tag druid-codex))
          (map #(frequencies (flatten %)) (vals (group-by-tag druid-codex))))

  ;; color test
  (reduce color-mix
          (mapcat (fn [[k v]]
                    (repeat v (tag->color k)))
                  (filter #(contains? tag->color (key %))
                          (:hurricane (make-tag-relationships druid-codex)))))

  (derive-color (:shred (make-tag-relationships druid-codex)))

  ;; all colors?
  (zipmap (keys (make-tag-relationships druid-all)) (map derive-color (vals (make-tag-relationships druid-all))))


  ;; generate dot file
  (loom.io/dot (loom.graph/weighted-graph (make-tag-relationships druid-codex)) "druid-codex.dot")
  (loom.io/dot (loom.graph/weighted-graph (make-tag-relationships druid-non-codex)) "druid-non-codex.dot")
  (loom.io/dot (loom.graph/weighted-graph (make-tag-relationships (concat druid-codex druid-non-codex))) "druid-all.dot")

  ;; links
  (get-links (remove-key-from-relationships (make-tag-relationships druid-codex)))

  ;; json
  (get-json druid-codex)
  (get-json druid-non-codex)
  (get-json druid-all))
