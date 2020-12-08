(ns clj-adventofcode2020.puzzle0
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.json :as json]))


(defn run-pt1
  []
  (let [data (->> (slurp "resources/puzzle0-ot.txt")
                  (json/read-str))]
    (->> (get data "members")
         (map second)
         (sort-by #(get % "local_score") >)
         (map-indexed #(str "*" (inc %1) "*. *Name*: " (get %2 "name") ", *Score*: " (get %2 "local_score") ", :star: " (get %2 "stars")))
         (reduce #(str %1 "\n" %2))
         (str "*OT Advent of Code 2020 Leaderboard*\n"))))
