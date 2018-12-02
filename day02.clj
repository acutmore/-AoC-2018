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

(defn different-by-one?
  [a b] (= 1 (count (filter identity (map not= a b)))))

(defn find-adjacent
  ([] nil)
  ([a] nil)
  ([a b & rest] (if (different-by-one? a b)
                  [a b]
                  nil)))

(defn process
  [list]
  (loop [rem list]
    (let [v (apply find-adjacent rem)]
      (if (nil? v)
        (recur (rest rem))
        v))))

(defn keep-same
  [a b] (if (= a b) a nil))

(def answer-part-b
  (delay
   (let [[code-a code-b] (process (sort codes))]
     (apply str
            (filter char?
                    (map keep-same code-a code-b))))))
