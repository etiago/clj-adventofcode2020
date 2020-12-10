(ns clj-adventofcode2020.puzzle11
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-puzzle-lines
  [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)
       (map #(Integer/valueOf %))))

(defonce puzzle11-example
  (->> (load-puzzle-lines "resources/puzzle11-example.txt")))

(defn run-pt1
  []

  )

(defn run-pt2
  []

  )
