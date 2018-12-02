(ns day01
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day01.txt"))

(def numbers (map bigint (str/split-lines raw-input)))

(def answer-part-a (delay (reduce + numbers)))

(def numbers-on-loop (flatten (repeat numbers)))

(def frequencies (reductions + numbers-on-loop))

(defn find-repeated-number [numbers]
  (let [seen (java.util.HashSet.)
        f (fn [remaining]
            (let [[head & tail] remaining]
              (if (.contains seen head)
                head
                (do
                  (.add seen head)
                  (recur tail)))))]
    (f numbers)))

(def answer-part-b (delay (find-repeated-number frequencies)))
