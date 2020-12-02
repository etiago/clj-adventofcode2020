(ns clj-adventofcode2020.puzzle1
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]))

;; Consciously eagerly loading file into memory.
;; Lazy sequence from a file makes the code slightly more ugly.
(def puzzle1-example
  (->> (slurp "resources/puzzle1.txt")
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(def puzzle1-real
  (->> (slurp "resources/puzzle1-real.txt")
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(defn run-pt1
  []
  (let [possible-combinations (combo/combinations puzzle1-real 2)
        sum-is-2020 (first
                     (take 1 (drop-while #(not= 2020 (+ (first %) (second %))) possible-combinations)))]
    (println (* (first sum-is-2020) (second sum-is-2020)))))

(defn run-pt2
  []
  (let [possible-combinations (combo/combinations puzzle1-real 3)
        sum-is-2020 (first
                     (take 1 (drop-while #(not= 2020 (+ (first %) (second %) (nth % 2))) possible-combinations)))]
    (println (* (first sum-is-2020) (second sum-is-2020) (nth sum-is-2020 2)))))
