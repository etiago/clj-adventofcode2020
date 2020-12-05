(ns clj-adventofcode2020.puzzle5
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn load-puzzle-file
  [filename]
  (-> (slurp filename)
      (clojure.string/split-lines)))

(def puzzle5-example
  (load-puzzle-file "resources/puzzle5-example.txt"))

(def puzzle5-real
  (load-puzzle-file "resources/puzzle5-real.txt"))

;; BFFFBBFRRR
;; FFFBBBFRRR
;; BBFFBBFRLL

(defn binary-search
  [left-edge right-edge walk-directions upper-char]
  (if (= 1 (count walk-directions))
    (if (= upper-char (first walk-directions))
      left-edge
      right-edge)
    (let [step (int (/ (- right-edge left-edge) 2))]
      (if (= upper-char (first walk-directions))
        (recur left-edge (dec (- right-edge step)) (rest walk-directions) upper-char)
        (recur (inc (+ left-edge step)) right-edge (rest walk-directions) upper-char)))))

(defn calculate-seat-id
  [directions]
  (let [row-directions (subs directions 0 7)
        col-directions (subs directions 7 (count directions))
        row-number (binary-search 0 127 row-directions \F)
        col-number (binary-search 0 7 col-directions \L)]
    (+ col-number (* row-number 8))))

(defn calculate-seat-row-col
  [directions]
  (let [row-directions (subs directions 0 7)
        col-directions (subs directions 7 (count directions))
        row-number (binary-search 0 127 row-directions \F)
        col-number (binary-search 0 7 col-directions \L)]
    { :row row-number :col col-number}))

(defn run-pt1
  []
  (reduce max (map calculate-seat-id puzzle5-real)))


(defn run-pt2
  []
  (filter #(= 1 (count (second %)))
          (apply merge
                 (map (fn [kv]
                        {(first kv)
                         (set/difference #{ 0 1 2 3 4 5 6 7 } (into #{} (map #(get % :col) (second kv))))})
                      (group-by #(get % :row) (map calculate-seat-row-col puzzle5-real))))))
