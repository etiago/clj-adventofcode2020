(ns clj-adventofcode2020.puzzle13
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-puzzle-lines
  [filename]
  (let [split-lines (str/split-lines
                     (slurp filename))
        id-strs (str/split (second split-lines) #",")
        ids (map #(Integer/parseInt %) (filter #(not= "x" %) id-strs))]
    {:earliest (Integer/parseInt (first split-lines))
     :ids (sort ids)}))

(defn load-puzzle-lines-pt2
  [filename]
  (let [split-lines (str/split-lines
                     (slurp filename))
        id-strs (str/split (second split-lines) #",")
        ids (map #(when-not (= "x" %) (Integer/parseInt %)) id-strs)]
    ids))

(defn puzzle13-example
  []
  (->>
   (load-puzzle-lines "resources/puzzle13-example.txt")))

(defn puzzle13-real
  []
  (->>
   (load-puzzle-lines "resources/puzzle13-real.txt")))

(defn puzzle13-example-pt2
  []
  (->>
   (load-puzzle-lines-pt2 "resources/puzzle13-example.txt")))

(defn puzzle13-example2-pt2
  []
  (->>
   (load-puzzle-lines-pt2 "resources/puzzle13-example2.txt")))

(defn puzzle13-real-pt2
  []
  (->>
   (load-puzzle-lines-pt2 "resources/puzzle13-real.txt")))

(defn iterate-and-add
  [num]
  (iterate #(+ % num) num))

(defn run-pt1
  []
  (let [current-puzzle (puzzle13-real)
        repeating-ids-seed (map #(iterate-and-add %) (:ids current-puzzle))
        earliest (:earliest current-puzzle)
        earliest-bus (reduce
     #(if (< (:diff %2) (:diff %1))
        %2
        %1)
     {:id -1 :diff Integer/MAX_VALUE}
     (map
      (fn
        [repeating-for-id]
        (let [partitioned (partition-by
                           #(< % earliest)
                           repeating-for-id)]
          
          {:id (first repeating-for-id)
           :diff (- (first (second partitioned)) earliest)}))
      repeating-ids-seed))]

    (* (:diff earliest-bus) (:id earliest-bus))))

;; (defn looping-with-offset
;;   [base number idx]
;;   (let [lcm-base-number (lcm base number)]
;;     ;(println lcm-base-number)
;;     (map
;;      #(- (* (inc %) lcm-base-number) (+ number idx))
;;      (range))))
;; 
;; (defn run-pt2
;;   []
;;     (let [current-puzzle (puzzle13-example-pt2)]
;;       (reduce
;;        set/intersection
;;        (set (take 2000000 (looping-with-offset 7 7 0)))
;;        (filter
;;         some?
;;         (map-indexed
;;          (fn [k v]
;;            (when (some? v)
;;              (do
;;                (println (str "k " k ", v " v))
;;                (set (take 2000000 (looping-with-offset 7 v k))))))
;;          current-puzzle))
;;        )))



