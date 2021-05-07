(ns clj-adventofcode2020.puzzle12
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-puzzle-lines
  [filename]
  (->> (slurp filename)
       (str/split-lines)
       (map #(identity [(keyword (subs % 0 1)) (Integer/parseInt (subs % 1))]))))

(defn puzzle12-example
  []
  (->>
   (load-puzzle-lines "resources/puzzle12-example.txt")))

(defn puzzle12-real
  []
  (->>
   (load-puzzle-lines "resources/puzzle12-real.txt")))

(def facing-order [:N :E :S :W])

(defn new-state-for-current-state-and-action
  [current-state step-action step-amount]
  (cond
    (= step-action :N) (merge current-state {:north-south (+ (:north-south current-state) step-amount)})
    (= step-action :S) (merge current-state {:north-south (- (:north-south current-state) step-amount)})
    (= step-action :E) (merge current-state {:east-west (+ (:east-west current-state) step-amount)})
    (= step-action :W) (merge current-state {:east-west (- (:east-west current-state) step-amount)})
    (= step-action :L) (merge current-state {:facing (mod (+ (:facing current-state) (- 360 step-amount)) 360)})
    (= step-action :R) (merge current-state {:facing (mod (+ (:facing current-state) step-amount) 360)})
    (= step-action :F) (new-state-for-current-state-and-action
                        current-state
                        (nth facing-order (/ (:facing current-state) 90))
                        step-amount)))

(defn path-reducer
  [current-state next-step]
  (let [step-action (first next-step)
        step-amount (second next-step)]
    (new-state-for-current-state-and-action current-state step-action step-amount)))

(defn run-pt1
  []
  (let [puzzle (puzzle12-real)
        final-state (reduce
                     path-reducer
                     {:east-west 0 :north-south 0 :facing 90}
                     puzzle)]
    (+ (Math/abs (:east-west final-state)) (Math/abs (:north-south final-state)))
    ))

(defn move-forward
  [current-state step-amount]
  (let [new-east-west (+ (:east-west current-state) (* (- (:wpt-ew current-state) (:east-west current-state)) step-amount))
        new-north-south (+ (:north-south current-state) (* (- (:wpt-ns current-state) (:north-south current-state)) step-amount))]
  (merge current-state
         {:east-west new-east-west
          :north-south new-north-south
          :wpt-ew (+ new-east-west (- (:wpt-ew current-state) (:east-west current-state)))
          :wpt-ns (+ new-north-south  (- (:wpt-ns current-state) (:north-south current-state)))})))

(defn denormalize-wpt-by-boat
  [current-state]
  (merge current-state {:wpt-ew (+ (:wpt-ew current-state) (:east-west current-state))
                        :wpt-ns (+ (:wpt-ns current-state) (:north-south current-state))}))

(defn normalize-wpt-by-boat
  [current-state]
  (merge current-state {:wpt-ew (- (:wpt-ew current-state) (:east-west current-state))
                        :wpt-ns (- (:wpt-ns current-state) (:north-south current-state))}))

(defn normalized-rotation-left
  [normalized-state]
  (merge normalized-state {:wpt-ew (* -1 (:wpt-ns normalized-state)) :wpt-ns (:wpt-ew normalized-state)}))

(defn rotation-left
  [current-state]
  (-> current-state
      (normalize-wpt-by-boat)
      (normalized-rotation-left)
      (denormalize-wpt-by-boat)))

(defn normalized-rotation-right
  [normalized-state]
  (merge normalized-state {:wpt-ew (:wpt-ns normalized-state) :wpt-ns (* -1 (:wpt-ew normalized-state))}))

(defn rotation-right
  [current-state]
  (-> current-state
      (normalize-wpt-by-boat)
      (normalized-rotation-right)
      (denormalize-wpt-by-boat)))

(defn rotate-waypoint-right
  [current-state step-amount]
  (cond
    (= step-amount 90) (merge current-state
                              {:wpt-ns (- (:wpt-ns current-state)
                                          (:north-south current-state))})

    (= step-amount 180) (merge current-state {:wpt-ns (- (:wpt-ns current-state) (:north-south current-state))
                                              :wpt-ew (- (:wpt-ew current-state) (:east-west current-state))})
    (= step-amount 270) (merge current-state {:wpt-ew (- (:wpt-ew current-state) (:east-west current-state))})))

(defn rotate-waypoint-left
  [current-state step-amount]
  (cond
    (= step-amount 90) (merge current-state {:wpt-ew (- (:wpt-ew current-state) (:east-west current-state))})

    (= step-amount 180) (merge current-state {:wpt-ns (- (:wpt-ns current-state) (:north-south current-state))
                                              :wpt-ew (- (:wpt-ew current-state) (:east-west current-state))})
    (= step-amount 270) (merge current-state {:wpt-ns (- (:wpt-ns current-state) (:north-south current-state))})))

(def action-to-fn-and-key
  {:N [+ :wpt-ns]
   :S [- :wpt-ns]
   :E [+ :wpt-ew]
   :W [- :wpt-ew]})

(defn move-waypoint
  [current-state step-action step-amount]
  (let [fn-and-key (get action-to-fn-and-key step-action)
        f (first fn-and-key)
        k (second fn-and-key)]
    (merge current-state {k (f (get current-state k) step-amount)})))

(defn new-state-for-current-state-and-action-pt2
  [current-state step-action step-amount]
  (cond
    (contains? action-to-fn-and-key step-action) (move-waypoint
                                                  current-state
                                                  step-action
                                                  step-amount)
    (= step-action :L) (reduce (fn [s _] (rotation-left s))
                               current-state
                               (range 0 (int (/ step-amount 90))))
    (= step-action :R) (reduce (fn [s _] (rotation-right s))
                               current-state
                               (range 0 (int (/ step-amount 90))))
    (= step-action :F) (move-forward current-state step-amount)))

(defn path-reducer-pt2
  [current-state next-step]
  (let [step-action (first next-step)
        step-amount (second next-step)]
    (new-state-for-current-state-and-action-pt2 current-state step-action step-amount)))

(defn run-pt2
  []
  (let [puzzle (puzzle12-real)
        final-state (reduce
                     path-reducer-pt2
                     {:east-west 0 :north-south 0 :wpt-ew 10 :wpt-ns 1 :facing 90}
                     puzzle)]
    (+ (Math/abs (:east-west final-state)) (Math/abs (:north-south final-state)))))
