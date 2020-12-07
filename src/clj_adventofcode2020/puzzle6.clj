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

(defn split-each-passenger-answers
  [group]
  (map (fn [passenger-answers]
         (clojure.string/split passenger-answers #"")) group))

(defn all-answers-to-set-by-group
  [all-answers]
  (map (fn [group-answers]
         (into #{} (flatten (split-each-passenger-answers group-answers))))
       all-answers))

(defn run-pt1
  []
  (->> puzzle6-real
       (all-answers-to-set-by-group)
       (map count)
       (reduce +)))

(defn split-each-passenger-answers-into-sets
  [group]
  (map (fn [passenger-answers]
         (into #{} (clojure.string/split passenger-answers #""))) group))
  
(defn run-pt2
  []
  (->> puzzle6-real
       (map (fn [passengers]
              (reduce clojure.set/intersection
                     (split-each-passenger-answers-into-sets passengers))))
       (map count)
       (reduce +)))
