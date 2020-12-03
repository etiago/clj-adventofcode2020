(ns clj-adventofcode2020.core
  (:gen-class)
  (:require [clj-adventofcode2020.puzzle1 :as puzzle1]
            [clj-adventofcode2020.puzzle2 :as puzzle2]
            [clj-adventofcode2020.puzzle3 :as puzzle3]))
;;             [clj-adventofcode2020.puzzle4 :as puzzle4]
;;             [clj-adventofcode2020.puzzle13 :as puzzle13]))

(defn -main
  [& args]
  (if (not= 3 (count args))
    (println "Pass in exactly three args. Puzzle number, part number and nr of times to run.")
    (let [puzzle (nth args 0)
          part (nth args 1)
          runs (Integer/valueOf (nth args 2))]
      (println
       (cond
         (and (= puzzle "1") (= part "1")) (println (puzzle1/run-pt1))
         (and (= puzzle "1") (= part "2")) (println (puzzle1/run-pt2))
         (and (= puzzle "2") (= part "1")) (println (puzzle2/run-pt1))
         (and (= puzzle "2") (= part "2")) (println (puzzle2/run-pt2))
         (and (= puzzle "3") (= part "2")) (println (puzzle3/run-pt1))
         (and (= puzzle "3") (= part "2")) (println (puzzle3/run-pt2))
         :else "Puzzle not found")))))
