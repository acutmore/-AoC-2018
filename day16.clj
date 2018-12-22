(ns day16
  (:require [clojure.string :as str]))

(def raw-input (slurp "./input/day16a.txt"))
(def raw-input-b (slurp "./input/day16b.txt"))

(def lines (str/split-lines raw-input))
(def lines-b (str/split-lines raw-input-b))

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
  (->> cpu
       (map (fn [[name operation]]
              {:opname name
               :result (cpu-call operation before instruction)
               :opcode (first instruction)}))
       (filter #(= after (:result %)))))

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

(def opcodes {0 (cpu :mulr)
              7 (cpu :bani)
              1 (cpu :eqri)
              4 (cpu :gtrr)
              15 (cpu :bori)
              13 (cpu :gtri)
              6 (cpu :borr)
              3 (cpu :eqrr)
              12 (cpu :addi)
              2  (cpu :setr)
              11 (cpu :gtir)
              9 (cpu :banr)
              5 (cpu :muli)
              14 (cpu :seti)
              10 (cpu :eqir)
              8 (cpu :addr)})

(def answer-part-b
  (delay ( (->> lines-b
                (map parse-four-digits)
                (reduce (fn [regs pc]
                          (let [opcode (first pc)
                                operation (opcodes opcode)]
                            (cpu-call operation regs pc)))
                        (vec [0 0 0 0]))))))

(defn set-set
  [prev v]
  (if (set? prev)
    (conj prev v)
    #{v}))

(comment
  ;--- Calculating opcode map for part -b
  (let [parseFn #(map parse-four-digits %)
        befores (parseFn (take-nth 4 lines))
        instructions (parseFn (take-nth 4 (rest lines)))
        afters (parseFn (take-nth 4 (nthrest lines 2)))
        samples (map list befores instructions afters)]
    (->> samples
         (map sample-matches)
         (flatten)
         (reduce (fn [map {:keys [opname opcode]}] (update map opcode set-set opname)) {})
         (clojure.pprint/pprint)))

  ; -- Above gives this map
  {0 #{:mulr}
   7 #{:bori :bani :gtir :eqir :borr :addr :gtrr :setr :eqri :muli}
   1 #{:eqir :borr :addr :seti :eqri}
   4 #{:seti :gtrr :gtri}
   15 #{:bori :borr}
   13 #{:eqrr :seti :gtri}
   6 #{:borr :mulr}
   3 #{:eqrr :seti}
   12 #{:bori :addi :borr :mulr}
   2 #{:bori :gtir :addi :borr :addr :seti :gtrr :setr :gtri}
   11 #{:gtir :eqir :eqrr :seti :gtrr :eqri :gtri}
   9
   #{:bori :bani :gtir :addi :borr :banr :addr :seti :gtrr :mulr :setr
     :gtri :muli}
   5 #{:bori :borr :mulr :muli}
   14 #{:bori :addi :borr :addr :seti :mulr}
   10 #{:bori :eqir :addi :borr :addr :eqrr :seti :gtrr :gtri}
   8 #{:bori :addi :borr :addr :mulr :muli}}

  ; -- Which can be reduced to the following by recursively eliminating entries with one opcode
  {0 :mulr
   7 :bani
   1 :eqri
   4 :gtrr
   15 :bori
   13 :gtri
   6 :borr
   3 :eqrr
   12 :addi
   2  :setr
   11 :gtir
   9 :banr
   5 :muli
   14 :seti
   10 :eqir
   8 :addr}
  ;---
  )