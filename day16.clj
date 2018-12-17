(ns day16
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day16a.txt"))

(def steps (str/split-lines raw-input))

(def four-digits #"(\d+),? (\d+),? (\d+),? (\d+)")

(defn parse-four-digits
  [s]
  (vec (map #(int (bigint %)) (rest (re-find four-digits s)))))

(defn bool-to-num
  [b] (if b 1 0))

(defn cpu-call
  [f regs [_ ra rb rc :as operands]]
  (f regs operands (map get [regs regs regs] [ra rb rc])))

(def cpu
  {:addr (fn [[r0 r1 r2 r3 :as regs] [_ va vb vc] [ra rb rc]]
           (assoc (vec regs) rc (+ ra rb)))
   :addi (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (+ ra vb)))
   :mulr (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (* ra rb)))
   :muli (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (* ra vb)))
   :banr (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bit-and ra rb)))
   :bani (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bit-and ra vb)))
   :borr (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bit-or ra rb)))
   :bori (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bit-or ra vb)))
   :setr (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc ra))
   :seti (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc va))
   :gtir (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bool-to-num (> va rb))))
   :gtri (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bool-to-num (> ra vb))))
   :gtrr (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bool-to-num (> ra rb))))
   :eqir (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bool-to-num (= va rb))))
   :eqri (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bool-to-num (= ra vb))))
   :eqrr (fn [[r0 r1 r2 r3 :as regs] [_ ra rb rc] [va vb vc]]
           (assoc (vec regs) rc (bool-to-num (= ra rb))))})

(defn sample-matches
  [[before instruction after]]
  (as-> cpu $
    (map second $)
    (map #(cpu-call % before instruction) $)
    (filter #(= after %) $)))

(defn answer-part-a
  []
  nil)

(let [parse #(map parse-four-digits %)
      befores (parse (take-nth 4 steps))
      instructions (parse (take-nth 4 (rest steps)))
      afters (parse (take-nth 4 (nthrest steps 2)))
      samples (map list befores instructions afters)]
  (as-> samples $
    (map sample-matches $)
    (map count $)
    (filter #(> % 2) $)
    (count $)))

; 537 - too low

; 605

; 674 - too high