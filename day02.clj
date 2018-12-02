(ns day02
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day02.txt"))

(def codes (str/split-lines raw-input))

(defn inc-or-one
  [v] (if (nil? v) 1 (inc v)))

(defn val-counts
  "Given a collection return a map of the vals to their occurrence count"
  [s]
  (reduce #(update %1 %2 inc-or-one) (sorted-map) s))

(defn repeat-counts
  [m] (filter #(> % 1) (set (vals m))))

(def answer-part-a
  (delay (let [char-counts (map #(repeat-counts (val-counts %)) codes)
               sum-counts (val-counts (flatten char-counts))]
           (* (get sum-counts 2) (get sum-counts 3)))))
