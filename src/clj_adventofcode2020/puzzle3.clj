(ns clj-adventofcode2020.puzzle3
  (:gen-class)
  (:require [clojure.string :as str]))

(defn puzzle-lines-to-list-of-lists-with-trees-true
  [puzzle-lines]
  (map
   (fn [line]
     (map #(= "#" %) (clojure.string/split line #"")))
   puzzle-lines))

(defn load-puzzle-file-to-list-of-lists-with-trees-true
  [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)
       puzzle-lines-to-list-of-lists-with-trees-true))

(def puzzle3-example
  (load-puzzle-file-to-list-of-lists-with-trees-true "resources/puzzle3-example.txt"))

(def puzzle3-real
  (load-puzzle-file-to-list-of-lists-with-trees-true "resources/puzzle3-real.txt"))

(defn is-tree?
  [puzzle x y]
  (nth (nth puzzle y) x))

(defn is-outside-to-the-right?
  [puzzle x]
  (>= x (count (first puzzle))))

(defn crossed-bottom?
  [puzzle y]
  (>= y (count puzzle)))

(defn x-or-scaled-back-x-to-fit
  [puzzle x]
  (if (is-outside-to-the-right? puzzle x)
    (recur puzzle (mod x (count (first puzzle))))
    x))

(defn sliding-coordinates
  [puzzle start-x-y sliding-delta-x-y]
  (let [new-x (+ (first start-x-y) (first sliding-delta-x-y))
        scaled-new-x (x-or-scaled-back-x-to-fit puzzle new-x)
        new-y (+ (second start-x-y) (second sliding-delta-x-y))]
    (cons start-x-y (lazy-seq (sliding-coordinates puzzle [scaled-new-x new-y] sliding-delta-x-y)))))

(defn sliding-coordinates-repeating-right-until-bottom
  [puzzle start-x-y sliding-delta-x-y]
  (take-while #(not (crossed-bottom? puzzle (second %))) (sliding-coordinates puzzle start-x-y sliding-delta-x-y)))

(defn count-trees-for-puzzle-and-slope
  [puzzle slope]
  (->> (sliding-coordinates-repeating-right-until-bottom puzzle [0 0] slope)
         (map #(is-tree? puzzle (first %) (second %)))
         (filter true?)
         (count)))

(defn run-pt1
  []
  (let [current-puzzle puzzle3-real]
    (count-trees-for-puzzle-and-slope current-puzzle [3 1])))

(defn run-pt2
  []
  (let [current-puzzle puzzle3-real
        slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (->> slopes
         (map #(count-trees-for-puzzle-and-slope current-puzzle %))
         (reduce *))))


