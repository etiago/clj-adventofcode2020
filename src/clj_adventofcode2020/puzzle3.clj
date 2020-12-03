(ns clj-adventofcode2020.puzzle3
  (:gen-class)
  (:require [clojure.string :as str]))

(def puzzle3-example
  (->> (slurp "resources/puzzle3-example.txt")
       (clojure.string/split-lines)
       (map (fn [line]
              (map #(= "#" %) (clojure.string/split line #""))))))

(def puzzle3-real
  (->> (slurp "resources/puzzle3-real.txt")
       (clojure.string/split-lines)
       (map (fn [line]
              (map #(= "#" %) (clojure.string/split line #""))))))

(defn is-tree?
  [puzzle x y]
  (nth (nth puzzle y) x))

(defn is-outside-to-the-right?
  [puzzle x y]
  (>= x (count (nth puzzle y))))

(defn apply-slope-slide
  [puzzle old-x old-y slope-x slope-y]
  [(+ slope-x old-x) (+ slope-y old-y)])

(defn is-at-bottom?
  [puzzle x y]
  (= (inc y) (count puzzle)))

(defn count-trees-to-bottom
  [puzzle x y tree-count slopes]
  (if (is-outside-to-the-right? puzzle x y)
    (count-trees-to-bottom puzzle (mod x (count (first puzzle))) y tree-count slopes)
    (let [new-tree-count (if (is-tree? puzzle x y)
                           (inc tree-count)
                           tree-count)]
      (if (is-at-bottom? puzzle x y)
        new-tree-count
        (let [new-x-y (apply-slope-slide puzzle x y (first slopes) (second slopes))]
          (count-trees-to-bottom puzzle (first new-x-y) (second new-x-y) new-tree-count slopes) )))))
    
(defn run-pt1
  []
  (count-trees-to-bottom puzzle3-real 0 0 0 [3 1]))

(defn run-pt2
  []
  (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
       (reduce * (map #(count-trees-to-bottom puzzle3-real 0 0 0 %) slopes))))
