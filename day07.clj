(ns day07
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day07.txt"))

(def steps (str/split-lines raw-input))

(def r #"Step (.) must be finished before step (.) can begin\.")

(defn parse-steps
  [s] (let [[_ dependency-step step] (re-find r s)]
        {:dependency-step dependency-step
         :step step}))

(def deps (map parse-steps steps))

(def alpha {"A" #{} "B" #{} "C" #{} "D" #{} "E" #{} "F" #{} "G" #{} "H" #{} "I" #{} "J" #{} "K" #{} "L" #{} "M" #{} "N" #{} "O" #{} "P" #{} "Q" #{} "R" #{} "S" #{} "T" #{} "U" #{} "V" #{} "W" #{} "X" #{} "Y" #{} "Z" #{}})

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn ready?
  [active-steps step-dep]
  (let [remaining (filter #(not (in? active-steps %)) (second step-dep))]
    (empty? remaining)))

(def answer-part-a
  (delay
   (let [step-deps (reduce #(update %1 (:step %2) conj (:dependency-step %2)) alpha deps)]
     (loop [order []]
       (let [next (->> step-deps
                       (filter (partial ready? order))
                       (map first)
                       (filter #(not (in? order %)))
                       (sort)
                       (first))]
         (if (nil? next)
           (str/join "" order)
           (recur (conj order next))))))))
