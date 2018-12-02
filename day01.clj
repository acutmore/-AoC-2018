(ns day01
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day01.txt"))

(def numbers (map bigint (str/split-lines raw-input)))

(def answer-part-a (delay (reduce + numbers)))

(def numbers-on-loop (flatten (repeat numbers)))

(def frequencies (reductions + numbers-on-loop))

(defn is-big-int [n] (= (type n) clojure.lang.BigInt))

(defn find-repeated-number [numbers]
  (let [find-repeat-reducer (fn [set v]
                              (if (contains? set v) v (conj set v)))]
    (take 1
          (filter is-big-int
                  (reductions find-repeat-reducer (sorted-set) numbers)))))

(def answer-part-b (delay (find-repeated-number frequencies)))
