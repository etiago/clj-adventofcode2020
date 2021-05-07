(ns clj-adventofcode2020.puzzle23
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]))

(defn parse-puzzle
  [puzzle]
  (-> puzzle
      (str/split #"")
      (as-> ns (map #(Integer/valueOf %) ns))))

(defn puzzle23-example
  []
  (vec (parse-puzzle "389125467")))

(def crab-grab-cnt 3)

(defn nth-many
  [coll indices]
  (vec
   (map
    #(nth coll %)
    indices)))

(defn get-left-cups
  [cups exclude-positions]
  (nth-many
   cups
   (sort (set/difference
          (set (range 0 (count cups)))
          (set exclude-positions)))))


(defn pick-cups
  [state]
  (let [cup-count (count (:cups state))
        cur-cup-idx (:cur-cup-idx state)
        cups-count-from-end (Math/min
                             crab-grab-cnt
                             (- (dec cup-count) cur-cup-idx))
        end-idx (+ 1 cur-cup-idx cups-count-from-end)
        cups-from-start (- crab-grab-cnt cups-count-from-end)
        take-start-positions (vec (range 0 cups-from-start))
        take-end-positions (vec (range (inc cur-cup-idx) end-idx))
        picked-positions (into [] cat [take-end-positions take-start-positions])]

    (assoc
     state
     :cups (get-left-cups (:cups state) picked-positions)
     :picked (nth-many
              (:cups state)
              picked-positions))))

(defn rotate-vector
  [v rotations]
  (vec (flatten [(subvec v rotations) (subvec v 0 rotations)])))

(defn insert-at
  [state position]
  (let [cups (:cups state)
        picked (:picked state)
        cur-cup-idx (:cur-cup-idx state)
        cups-inserted (vec (flatten [(subvec cups 0 (inc position))
                                     picked
                                     (subvec cups (inc position))]))
        cups-rotated-if-needed (if (< position cur-cup-idx)
                                 (rotate-vector cups-inserted 3)
                                 cups-inserted)]
    (assoc state
           :picked
           []
           :cups
           cups-rotated-if-needed
           )))

(defn get-position-to-insert
  ([state]
   (let [potential-insert-position (dec (get (:cups state) (:cur-cup-idx state)))]
     (get-position-to-insert state potential-insert-position)))
  ([state insert-to-check]
   (if (< insert-to-check 1)
     (recur state (apply max (:cups state)))
     (if (= -1 (.indexOf (:cups state) insert-to-check))
       (recur state (dec insert-to-check))
       (.indexOf (:cups state) insert-to-check)))))

(defn do-moves
  [state moves]
  (println (str "About to do moves, state: " state))
  (let [state-with-picked-cups (pick-cups state)
        _ (println (str "after picking: " state-with-picked-cups))
        position-to-insert (get-position-to-insert state-with-picked-cups)
        _ (println (str "insert at: " position-to-insert))
        state-with-inserted-cups (insert-at state-with-picked-cups position-to-insert)
        state-with-next-idx (update
                             state-with-inserted-cups
                             :cur-cup-idx
                             #(mod (inc %) (- (count (:cups state)) 3)))
        moves-left (dec moves)]

    ;(println state-with-next-idx)
    (if (= 0 moves-left)
      state-with-next-idx
      (recur state-with-next-idx (dec moves)))))

(defn run-pt1
  [runs]
  (let [current-puzzle (puzzle23-example)
        initial-state {:cups current-puzzle :picked [] :cur-cup-idx 0}
        final-puzzle (do-moves initial-state runs)]
    final-puzzle
  ))
