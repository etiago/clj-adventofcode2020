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
  [puzzle x y tree-count slope-x slope-y]
  (if (is-outside-to-the-right? puzzle x y)
    (count-trees-to-bottom puzzle (mod x (count (first puzzle))) y tree-count slope-x slope-y)
    (let [new-tree-count (if (is-tree? puzzle x y)
                           (inc tree-count)
                           tree-count)]
      (if (is-at-bottom? puzzle x y)
        new-tree-count
        (let [new-x-y (apply-slope-slide puzzle x y slope-x slope-y)]
          (count-trees-to-bottom puzzle (first new-x-y) (second new-x-y) new-tree-count slope-x slope-y) )))))
    
(defn run-pt1
  []
  
  )

(defn run-pt2
  []
  )
