(ns clj-adventofcode2020.puzzle6
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn load-puzzle-file
  [filename]
  (-> (slurp filename)
      (clojure.string/split #"\n\n")))

(defn parse-group
  [group-str]
  (clojure.string/split group-str #"\n"))

(def puzzle6-example
  (->> (load-puzzle-file "resources/puzzle6-example.txt")
       (map parse-group)))

(def puzzle6-real
  (->> (load-puzzle-file "resources/puzzle6-real.txt")
       (map parse-group)))

(defn run-pt1
  []
  (->> puzzle6-real
       (map (fn [passengers]
              (into #{} (flatten (map #(clojure.string/split % #"") passengers)))))
       (map count)
       (reduce +)))

(defn run-pt2
  []
  (->> puzzle6-real
       (map (fn [passengers]
              (apply clojure.set/intersection (map #(into #{} (clojure.string/split % #"")) passengers))))
       (map count)
       (reduce +)))
