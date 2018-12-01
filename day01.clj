(ns day01
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day01.txt"))

(def numbers (map bigint (str/split-lines raw-input)))

(def answer-part-a (reduce + numbers))

(def numbers-on-loop (flatten (repeat numbers)))

(def frequencies (reductions + numbers-on-loop))

(defn find-repeated-number []
  (let [seen (java.util.HashSet.)]
    (fn [numbers]
      (let [[head & tail] numbers]
        (if (.contains seen head)
          head
          (do
            (.add seen head)
            (recur tail)))))))

(def answer-part-b ((find-repeated-number) frequencies))
