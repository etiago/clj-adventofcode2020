(ns clj-adventofcode2020.puzzle22
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]))

(defn extract-player-deck
  [player-str]
  (map #(Integer/valueOf %) (str/split (second (str/split player-str #"\n" 2)) #"\n")))

(defn load-puzzle-lines
  [filename]
  (-> (slurp filename)
      (str/split #"\n\n")
      (as-> parts (map #(extract-player-deck %) parts))))

(defn play-to-end
  [current-puzzle]
  (loop [player-decks current-puzzle]
    (let [player-one (first player-decks)
          player-two (second player-decks)
          first-player-one (first player-one)
          first-player-two (first player-two)
          rest-player-one (rest player-one)
          rest-player-two (rest player-two)]
      (if (or (empty? player-one)
              (empty? player-two))
        (first (filter not-empty player-decks))
        (if (> first-player-one first-player-two)
          (recur (seq [(concat rest-player-one [first-player-one first-player-two])
                       rest-player-two]))
          (recur (seq [rest-player-one
                       (concat rest-player-two [first-player-two first-player-one])])))))))

(defn calculate-score
  [winning-deck]
  (reduce + (map-indexed #(* (inc %1) %2) (reverse winning-deck))))

(defn run-pt1
  []
  (let [current-puzzle (load-puzzle-lines "resources/puzzle22-real.txt")
        winning-deck (play-to-end current-puzzle)]
    (calculate-score winning-deck)))

(defn run-pt2
  []

  )
