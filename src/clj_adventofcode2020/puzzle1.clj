(ns clj-adventofcode2020.puzzle1
  (:gen-class))

;; Consciously eagerly loading file into memory.
;; Lazy sequence from a file makes the code slightly more ugly.
(def puzzle-input
  (->> (slurp "resources/puzzle1.txt")
       (clojure.string/split-lines)
       (map #(Double/parseDouble %))))

(defn run-pt1
  []
  (println "Ran part one"))

(defn run-pt2
  []
  (println "Ran part two"))
