(ns clj-adventofcode2020.puzzle9
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-puzzle-lines
  [filename look-back-qty]
  (let [lines-str (clojure.string/split-lines (slurp filename))
        lines (vec (map #(clojure.lang.BigInt/fromBigInteger (BigInteger. %)) lines-str))
        preamble (vec (subvec lines 0 look-back-qty))
        inputs (subvec lines look-back-qty)]
    {:preamble preamble :inputs inputs}))

(def puzzle9-example
  (->> (load-puzzle-lines "resources/puzzle9-example.txt" 25)))

(def puzzle9-example2
  (->> (load-puzzle-lines "resources/puzzle9-example2.txt" 5)))

(def puzzle9-real
  (->> (load-puzzle-lines "resources/puzzle9-real.txt" 25)))

(defn sum-pair-exists?
  [input preamble]
  (first (filter #(true? (second %)) (map #(identity [% (= input (+ (first %) (second %)))]) (combo/combinations preamble 2)))))

(defn run-pt1
  []
  (loop [preamble-and-inputs puzzle9-real]
    (let [{preamble :preamble
           inputs :inputs} preamble-and-inputs
          next-input (first inputs)
          rest-inputs (rest inputs)]
      (if-not (sum-pair-exists? next-input preamble)
        next-input
        (recur {:preamble (subvec (conj preamble next-input) 1 26) :inputs rest-inputs})))))
          

(defn find-contiguous-adding-to-number
  [full-inputs number]
  true
  )

(defn run-pt2
  []
  (let [full-inputs (concat (:preamble puzzle9-real) (:inputs puzzle9-real))]
    (loop [current-sum-list [(first full-inputs)]
           inputs-left (rest full-inputs)
           current-sum (first full-inputs)]
      (if (= (bigint 1721308972) (+ current-sum (first inputs-left)))
        (let [solution-list (conj current-sum-list (first inputs-left))
              solution-max (apply max solution-list)
              solution-min (apply min solution-list)]
          (+ solution-min solution-max))
        (if (< (bigint 1721308972) (+ current-sum (first inputs-left)))
          (recur (into [] (rest current-sum-list)) inputs-left (- current-sum (first current-sum-list)))
          (recur (conj current-sum-list (first inputs-left))
                 (rest inputs-left)
                 (+ current-sum (first inputs-left))))))))
