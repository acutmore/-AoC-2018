(ns day03
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day03.txt"))

(def orders (str/split-lines raw-input))

(defstruct order :id :x :y :width :height)

(defn parse-order
  [s] (let [[_ id x y width height] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)]
        (struct-map order
          :id id
          :x (bigint x)
          :y (bigint y)
          :width (bigint width)
          :height (bigint height))))

(defn overlap-1d
  [start-a width-a start-b width-b]
  (let [start (max start-a start-b)
        end (min (+ start-a width-a) (+ start-b width-b))]
    [start end]))

(defn overlap-2d
  [{xa :x, ya :y, wa :width, ha :height}
   {xb :x, yb :y, wb :width, hb :height}]
  (let [[start end] (overlap-1d xa wa xb wb)
        [start2 end2] (overlap-1d ya ha yb hb)]
    (for [e1 (range start end)
          e2 (range start2 end2)]
      (list e1 e2))))

(defn tails
  [col]
  (reductions (fn [s _] (rest s)) col col))

(defn overlaps
  [c]
  (let [[v & col] c]
    (mapcat #(overlap-2d v %) col)))

(def answer-part-a
  (delay (count (set (mapcat overlaps (tails (map parse-order orders)))))))

(def all-ids (reduce conj #{} (map #(:id (parse-order %)) orders)))

(defn overlap-ids
  [{ida :id, xa :x, ya :y, wa :width, ha :height}
   {idb :id, xb :x, yb :y, wb :width, hb :height}]
  (let [[start end] (overlap-1d xa wa xb wb)
        [start2 end2] (overlap-1d ya ha yb hb)]
    (for [e1 (range start end)
          e2 (range start2 end2)]
      [ida idb])))

(defn all-overlap-ids
  [c]
  (let [[v & col] c]
    (mapcat #(overlap-ids v %) col)))

(def answer-part-b
  (delay
   (reduce disj all-ids
           (flatten
            (mapcat all-overlap-ids
                    (tails (map parse-order orders)))))))
