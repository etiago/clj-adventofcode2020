(ns clj-adventofcode2020.puzzle7
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn qty-color-str-to-map
  [qty-color-str]
  (when (not= qty-color-str "no other")
    (let [split-str (clojure.string/split qty-color-str #" " 2)
          qty (Integer/valueOf (first split-str))
          color (second split-str)]
      {color qty})))

(defn contains-list-to-map
  [list]
  (let [parsed-contained
        (as-> (second list) contained
          (clojure.string/split contained #", ")
          (map qty-color-str-to-map contained)
          (apply merge contained))]
    {(first list) parsed-contained}))

(defn load-puzzle-file
  [filename]
  (-> (slurp filename)
      (clojure.string/replace #"\." "")
      (clojure.string/replace #"[ ]*bag[s]*" "")
      (clojure.string/split #"\n")
      (as-> p (map #(clojure.string/split % #" contain ") p))
      (as-> p (map contains-list-to-map p))
      (as-> p (apply merge p))))

(def puzzle7-example
  (->> (load-puzzle-file "resources/puzzle7-example.txt")
       ))

(def puzzle7-example2
  (->> (load-puzzle-file "resources/puzzle7-example2.txt")
       ))

(def puzzle7-real
  (->> (load-puzzle-file "resources/puzzle7-real.txt")
       ))

(defn contains-color-starting-on-color?
  [color-map color-start color-find to-check]
  (let [children (get color-map color-start)]
    (if (and (empty? to-check) (empty? children))
      false
      (do
        ;;(println (str "checking children: " children ", to-chec: " to-check ", color" color-find))
        (if (contains? children color-find)
          true
          (recur color-map (first to-check) color-find (concat (rest to-check) (keys children))))))))

(defn children-to-repeated-list
  [children]
  (into [] (reduce concat (map #(repeat (second %) (first %)) children))))

(defn count-visited
  [puzzle to-visit visited-count]
  (if (empty? to-visit)
    visited-count
    (let [first-el (first to-visit)
          children (get puzzle first-el)]
      (recur puzzle (into (rest to-visit) (children-to-repeated-list children)) (inc visited-count)))))


(defn run-pt1
  []
  (let [current-puzzle puzzle7-real
        all-colors (keys current-puzzle)]
    (count (filter true? (map #(contains-color-starting-on-color? current-puzzle % "shiny gold" []) all-colors)))))

    ;(contains-color-starting-on-color? current-puzzle "dotted black" "shiny gold" [])))

(defn run-pt2
  []
  (count-visited puzzle7-real ["shiny gold"] 0))
