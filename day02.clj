(ns day02
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day02.txt"))

(def codes (str/split-lines raw-input))

(defn unique-frequencies
  [c] (into [] (set (vals (frequencies c)))))

(def answer-part-a
  (delay (let [char-counts (map unique-frequencies codes)
               sum-counts (frequencies (flatten char-counts))]
           (* (get sum-counts 2) (get sum-counts 3)))))

(defn different-by-one?
  [a b] (= 1 (get (frequencies (map = a b)) false)))

(defn find-adjacent
  ([] nil)
  ([a] nil)
  ([a b & rest] (if (different-by-one? a b)
                  [a b]
                  nil)))

(defn search
  "Starting at the beggining of the collection, pass the remander of the list to the provided
   function until it returns a non-nil result. Returns the result (if any)"
  [f col]
  (loop [remaining col]
    (if (empty? remaining)
      nil
      (let [result (apply f remaining)]
        (if (nil? result)
          (recur (rest remaining))
          result)))))

(defn keep-if-same
  [a b] (if (= a b) a nil))

(def answer-part-b
  (delay
   (let [[code-a code-b] (search find-adjacent (sort codes))]
     (apply str
            (filter char?
                    (map keep-if-same code-a code-b))))))
