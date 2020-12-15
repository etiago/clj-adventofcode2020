(ns clj-adventofcode2020.puzzle15
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]))

(defn load-puzzle-lines
  [filename]
  (let [split-lines (str/split-lines
                     (slurp filename))]

    (->
     (first split-lines)
     (str/split #",")
     (as-> s (map #(Integer/valueOf %) s)))))

(defn puzzle15-example
  []
  (->>
   (load-puzzle-lines "resources/puzzle15-example.txt")))

(defn puzzle15-real
  []
  (->>
   (load-puzzle-lines "resources/puzzle15-real.txt")))

(defn was-never-spoken?
  [to-speak seen-at]
  (not (contains? seen-at to-speak)))

(defn should-speak-next
  [to-speak seen-at current-index]
  (- current-index (first (get seen-at to-speak))))

(defn get-new-seen-at
  [to-speak seen-at current-index]
  (let [positions-current-to-speak (get seen-at to-speak)
        new-positions [current-index (first positions-current-to-speak)]]
    (merge seen-at {to-speak new-positions})))

(defn run-pt1
  []
  (let [current-puzzle (puzzle15-real)]

    (reductions
     (fn [state current-index]
       (let [{seen-at :seen-at to-speak :to-speak} state
             about-to-speak (if (was-never-spoken? to-speak seen-at)
                              0
                              (should-speak-next to-speak seen-at current-index))
             new-seen-at (get-new-seen-at to-speak seen-at current-index)]

         (merge state {:seen-at new-seen-at :to-speak about-to-speak})))
         
           
     {:seen-at (zipmap (butlast current-puzzle) (map #(identity [% %]) (drop 1 (range))))
      :to-speak (last current-puzzle)}
     (drop (count current-puzzle) (range)))
     
    
  ))
