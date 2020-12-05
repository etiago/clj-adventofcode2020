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

(defn go-left?
  [walk-directions upper-char]
  (= upper-char (first walk-directions)))

(defn calculate-step
  [left-edge right-edge]
  (int (/ (- right-edge left-edge) 2)))
  
(defn binary-search
  [left-edge right-edge walk-directions upper-char]
  (if (= 1 (count walk-directions))
    (if (go-left? walk-directions upper-char)
      left-edge
      right-edge)
    (let [step (calculate-step left-edge right-edge)]
      (if (go-left? walk-directions upper-char)
        (recur left-edge (dec (- right-edge step)) (rest walk-directions) upper-char)
        (recur (inc (+ left-edge step)) right-edge (rest walk-directions) upper-char)))))

(defn calculate-seat-row-col
  [directions]
  (let [row-directions (subs directions 0 7)
        col-directions (subs directions 7)
        row-number (binary-search 0 127 row-directions \F)
        col-number (binary-search 0 7 col-directions \L)]
    {:row row-number :col col-number}))

(defn calculate-seat-id
  [directions]
  (let [row-col (calculate-seat-row-col directions)]
    (+ (get row-col :col) (* (get row-col :row) 8))))

(defn unpack-col-from-values
  [kv]
  [(first kv) (into #{} (map #(get % :col) (second kv)))])

(defn missing-cols
  [kv]
  (println kv)
  {(first kv)
   (set/difference
    #{ 0 1 2 3 4 5 6 7 } (second kv))})

(defn run-pt1
  []
  (reduce max (map calculate-seat-id puzzle5-real)))


(defn run-pt2
  []
  (->> puzzle5-real
       (map calculate-seat-row-col)
       (group-by #(get % :row))
       (map unpack-col-from-values)
       (map missing-cols)
       (apply merge)
       (filter #(= 1 (count (second %))))))
