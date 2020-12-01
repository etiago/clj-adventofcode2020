(ns clj-adventofcode2020.puzzle1
  (:gen-class))

;; Consciously eagerly loading file into memory.
;; Lazy sequence from a file makes the code slightly more ugly.
(def puzzle-input
  (->> (slurp "resources/puzzle1-real.txt")
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(defn first-plus-every-other
  [first-int ints]
  (if (empty? ints) nil
      (lazy-seq
       (cons (+ first-int (first ints))
             (first-plus-every-other first-int (rest ints))))))

(defn combinations-of-first-plus-every-other
  [ints start-pos]
  (if (empty? ints) nil
      (lazy-seq
       (concat (map-indexed #(list [start-pos (+ start-pos (inc %1))] %2) (first-plus-every-other (first ints) (rest ints)))
               (combinations-of-first-plus-every-other (rest ints) (inc start-pos))))))

(defn take-until-sum-is-2020
  [ints]
  (first (take 1 (drop-while #(not= 2020 (second %)) (combinations-of-first-plus-every-other ints 0)))))


(defn run-pt1
  []
  (println "Ran part one")
  (let [positions (first (take-until-sum-is-2020 puzzle-input))]
    (println (* (nth puzzle-input (first positions)) (nth puzzle-input (second positions))))))

(defn run-pt2
  []
  (println "Ran part two"))
