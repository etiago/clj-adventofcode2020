(ns clj-adventofcode2020.core
  (:gen-class)
  (:require
   ;;[clj-adventofcode2020.puzzle0 :as puzzle0]
   ;;[clj-adventofcode2020.puzzle1 :as puzzle1]
   ;;[clj-adventofcode2020.puzzle2 :as puzzle2]
   ;;[clj-adventofcode2020.puzzle3 :as puzzle3]
   ;;[clj-adventofcode2020.puzzle4 :as puzzle4]
   ;;[clj-adventofcode2020.puzzle5 :as puzzle5]
   ;;[clj-adventofcode2020.puzzle6 :as puzzle6]
   ;;[clj-adventofcode2020.puzzle7 :as puzzle7]
   ;;[clj-adventofcode2020.puzzle8 :as puzzle8]
   ;;[clj-adventofcode2020.puzzle9 :as puzzle9]
   ;;[clj-adventofcode2020.puzzle10 :as puzzle10]
   [clj-adventofcode2020.puzzle11 :as puzzle11]
   [clj-adventofcode2020.puzzle12 :as puzzle12]
   [clj-adventofcode2020.puzzle13 :as puzzle13]
   [clj-adventofcode2020.puzzle15 :as puzzle15]))

(defn -main
  [& args]
  (if (not= 3 (count args))
    (println "Pass in exactly three args. Puzzle number, part number and nr of times to run.")
    (let [puzzle (nth args 0)
          part (nth args 1)
          runs (Integer/valueOf (nth args 2))
          available-puzzles [0 1 2 3 4 5 6 7 8 9 10 11 12 13 15]]
      (println
       (if (some #(= % (Integer/valueOf puzzle)) available-puzzles)
         (let [f (resolve
                  (symbol
                   (str "clj-adventofcode2020.puzzle" puzzle "/run-pt" part)))]
           (f))
         "Puzzle not found")))))
