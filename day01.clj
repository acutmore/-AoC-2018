(ns day01
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day01.txt"))

(def numbers (map bigint (str/split-lines raw-input)))

(def answer-part-a (delay (reduce + numbers)))

(def numbers-on-loop (flatten (repeat numbers)))

(def frequencies (reductions + numbers-on-loop))

(defn find-duplicate
  [col]
  (loop [seen (sorted-set)
         remaining col]
    (let [[head & tail] remaining]
      (if (contains? seen head)
        head
        (recur (conj seen head) tail)))))

(def answer-part-b (delay (find-duplicate frequencies)))
