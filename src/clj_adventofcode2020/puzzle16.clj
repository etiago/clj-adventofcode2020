(ns clj-adventofcode2020.puzzle16
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]))

(defn third
  [s]
  (nth s 2))

(defn extract-min-max
  [part]
  (vec (map #(Integer/valueOf %) (str/split part #"-"))))

(defn rules-str-parser
  [rules-str]
  (let [rule-parts (str/split rules-str #": ")
        rule-name (keyword (first rule-parts))
        rule-intervals (map extract-min-max (str/split (second rule-parts) #" or "))]
    {rule-name (vec rule-intervals)}))

(defn extract-rules
  [all-rules-strs]
  (into {}
   (map
    rules-str-parser
    all-rules-strs)))

(defn extract-ticket
  [ticket-str]
  (map #(Integer/valueOf %) (str/split ticket-str #",")))

(defn parse-tickets
  [ticket-strs]
  (map extract-ticket ticket-strs))

(defn load-puzzle-lines
  [filename]
  (let [split-lines (map
                     str/split-lines
                     (str/split
                      (slurp filename)
                      #"\n\n"))
        all-rules-strs (first split-lines)
        our-ticket-strs (second split-lines)
        other-tickets-strs (third split-lines)
        rules-parsed (extract-rules all-rules-strs)
        our-ticket (first (parse-tickets (rest our-ticket-strs)))
        other-tickets (parse-tickets (rest other-tickets-strs))]
    
    {:rules rules-parsed
     :our-ticket our-ticket
     :other-tickets other-tickets}))

(defn puzzle16-example
  []
  (->>
   (load-puzzle-lines "resources/puzzle16-example.txt")))

(defn puzzle16-real
  []
  (->>
   (load-puzzle-lines "resources/puzzle16-real.txt")))

(defn ticket-value-is-valid-for-rule?
  [ticket-value rule-entry]
  (let [rule-name (first rule-entry)
        rule-boundaries (second rule-entry)]
    [rule-name
     (reduce
      #(or %1 %2)
      false
      (map
       #(and (>= ticket-value (first %)) (<= ticket-value (second %)))
       rule-boundaries))]))

(defn each-ticket-field-valid-for-at-least-one-rule?
  [ticket rules]
  (loop [rules-left rules
         ticket-left ticket]
    (let [next-ticket-value (first ticket-left)
          ticket-field-is-valid-per-rule (map
                                          #(ticket-value-is-valid-for-rule? next-ticket-value %)
                                          rules-left)
          first-valid-rule (first (filter
                            #(true? (second %))
                            ticket-field-is-valid-per-rule))]
      ;(println ticket-field-is-valid-per-rule)
      ;(println (str "first valid rule:" first-valid-rule ", ticket-value " next-ticket-value))
      (if (nil? first-valid-rule)
        next-ticket-value
        (if (or (= 1 (count rules-left)) (= 1 (count ticket-left)))
          true
          (recur (dissoc rules-left (first first-valid-rule))
                 (rest ticket-left))))

      )))

(defn run-pt1
  []
  (let [current-puzzle (puzzle16-real)
        rules (:rules current-puzzle)
        sorted-other-tickets (map sort (:other-tickets current-puzzle))]
    (reduce
     +
     (filter
     number?
     (map
      #(each-ticket-field-valid-for-at-least-one-rule? % rules)
      sorted-other-tickets)))
  ))

(defn each-ticket-field-valid-for-at-least-one-rule?-pt2
  [ticket rules]
  (loop [rules-left rules
         ticket-left ticket]
    (let [next-ticket-value (first ticket-left)
          ticket-field-is-valid-per-rule (map
                                          #(ticket-value-is-valid-for-rule? next-ticket-value %)
                                          rules-left)
          first-valid-rule (first (filter
                            #(true? (second %))
                            ticket-field-is-valid-per-rule))]
      ;(println ticket-field-is-valid-per-rule)
      ;(println (str "first valid rule:" first-valid-rule ", ticket-value " next-ticket-value))
      (if (nil? first-valid-rule)
        next-ticket-value
        (if (or (= 1 (count rules-left)) (= 1 (count ticket-left)))
          true
          (recur (dissoc rules-left (first first-valid-rule))
                 (rest ticket-left))))

      )))

(defn run-pt2
  []

  )
