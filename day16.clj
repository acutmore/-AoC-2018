(ns day16
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day16a.txt"))

(def lines (str/split-lines raw-input))

(def four-digits #"(\d+),? (\d+),? (\d+),? (\d+)")

(defn parse-four-digits
  [s]
  (vec (map #(int (bigint %)) (rest (re-find four-digits s)))))

(defn bool-to-num
  [b] (if b 1 0))

(defn cpu-call
  [f regs [_ va vb rc]]
  (let [[ra rb] (map get [regs regs] [va vb])
        result (f [ra rb] [va vb])]
    (assoc regs rc result)))

(def cpu
  {:addr (fn [[ra rb] [va vb]] (+ ra rb))
   :addi (fn [[ra rb] [va vb]] (+ ra vb))
   :mulr (fn [[ra rb] [va vb]] (* ra rb))
   :muli (fn [[ra rb] [va vb]] (* ra vb))
   :banr (fn [[ra rb] [va vb]] (bit-and ra rb))
   :bani (fn [[ra rb] [va vb]] (bit-and ra vb))
   :borr (fn [[ra rb] [va vb]] (bit-or ra rb))
   :bori (fn [[ra rb] [va vb]] (bit-or ra vb))
   :setr (fn [[ra rb] [va vb]] ra)
   :seti (fn [[ra rb] [va vb]] va)
   :gtir (fn [[ra rb] [va vb]] (bool-to-num (> va rb)))
   :gtri (fn [[ra rb] [va vb]] (bool-to-num (> ra vb)))
   :gtrr (fn [[ra rb] [va vb]] (bool-to-num (> ra rb)))
   :eqir (fn [[ra rb] [va vb]] (bool-to-num (= va rb)))
   :eqri (fn [[ra rb] [va vb]] (bool-to-num (= ra vb)))
   :eqrr (fn [[ra rb] [va vb]] (bool-to-num (= ra rb)))})

(defn sample-matches
  [[before instruction after]]
  (as-> cpu $
    (map #(vec [(first %) (cpu-call (second %) before instruction)]) $)
    (filter #(= after (second %)) $)))

(def answer-part-a
  (delay (let [parse #(map parse-four-digits %)
               befores (parse (take-nth 4 lines))
               instructions (parse (take-nth 4 (rest lines)))
               afters (parse (take-nth 4 (nthrest lines 2)))
               samples (map list befores instructions afters)]
           (->> samples
                (map sample-matches)
                (map count)
                (filter #(> % 2))
                (count)))))
