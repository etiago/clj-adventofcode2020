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
  (load-puzzle-file "resources/puzzle7-example.txt"))

(def puzzle7-example2
  (load-puzzle-file "resources/puzzle7-example2.txt"))

(def puzzle7-deepexample
  (load-puzzle-file "resources/puzzle7-deepexample.txt"))

(def puzzle7-real
  (load-puzzle-file "resources/puzzle7-real.txt"))

(defn contains-color-starting-on-color?
  [color-map color-start color-find to-check]
  (let [children (get color-map color-start)]
    (when-not (every? empty? [to-check children])
      (if (contains? children color-find)
        true
        (recur
         color-map
         (first to-check)
         color-find
         (concat (rest to-check) (keys children)))))))

(defn children-to-repeated-list
  [children]
  (flatten (map #(repeat (second %) (first %)) children)))

(defn count-visited
  [puzzle to-visit visited-count]
  (if (empty? to-visit)
    visited-count
    (let [children (get puzzle (first to-visit))]
      (recur
       puzzle
       (into (rest to-visit) (children-to-repeated-list children))
       (inc visited-count)))))

(defn contains-color-starting-on-color-fn
  [puzzle starting-color]
  (fn [color]
    (contains-color-starting-on-color? puzzle color starting-color [])))

(defn run-pt1
  []
  (let [current-puzzle puzzle7-real
        all-colors (keys current-puzzle)]
    (->> (keys current-puzzle)
         (map (contains-color-starting-on-color-fn current-puzzle "shiny gold"))
         (filter true?)
         (count))))

(defn run-pt2
  []
  (count-visited puzzle7-deepexample ["shiny gold"] 0))
