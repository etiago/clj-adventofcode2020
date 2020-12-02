(ns clj-adventofcode2020.puzzle2
  (:gen-class)
  (:require [clojure.string :as str]))

(def puzzle2-example
  (->> (slurp "resources/puzzle2.txt")
       (clojure.string/split-lines)
       (map (fn [line]
              (let [line-parts (str/split line #": ")
                    policy-part (first line-parts)
                    password (second line-parts)
                    policy-split-parts (str/split policy-part #" ")
                    policy-letter (first (char-array (second policy-split-parts)))
                    policy-min-max-parts (str/split (first policy-split-parts) #"-")
                    policy-min (Integer/parseInt (first policy-min-max-parts))
                    policy-max (Integer/parseInt (second policy-min-max-parts))]
                {:policy { :min policy-min :max policy-max :letter policy-letter }
                 :password password})))))

(def puzzle2-real
  (->> (slurp "resources/puzzle2-real.txt")
       (clojure.string/split-lines)
       (map (fn [line]
              (let [line-parts (str/split line #": ")
                    policy-part (first line-parts)
                    password (second line-parts)
                    policy-split-parts (str/split policy-part #" ")
                    policy-letter (first (char-array (second policy-split-parts)))
                    policy-min-max-parts (str/split (first policy-split-parts) #"-")
                    policy-min (Integer/parseInt (first policy-min-max-parts))
                    policy-max (Integer/parseInt (second policy-min-max-parts))]
                {:policy { :min policy-min :max policy-max :letter policy-letter }
                 :password password})))))

(defn password-is-valid?
  [policy-and-password]
  (let [policy-letter (get-in policy-and-password [:policy :letter])
        policy-min (get-in policy-and-password [:policy :min])
        policy-max (get-in policy-and-password [:policy :max])
        password (get-in policy-and-password [:password])
        policy-letter-occurrences-in-password (count (filter #(= policy-letter %) password))]
    (and (>= policy-letter-occurrences-in-password policy-min)
         (<= policy-letter-occurrences-in-password policy-max))))
    
(defn run-pt1
  []
  (println (count (filter true? (map password-is-valid? puzzle2-real)))))

(defn password-is-valid-positionally?
  [policy-and-password]
  (let [policy-letter (get-in policy-and-password [:policy :letter])
        policy-min (get-in policy-and-password [:policy :min])
        policy-max (get-in policy-and-password [:policy :max])
        password (get-in policy-and-password [:password])
        policy-letter-occurrences-in-password (count (filter #(= policy-letter %) password))
        min-matches (= policy-letter (nth password (dec policy-min)))
        max-matches (= policy-letter (nth password (dec policy-max)))]
    (if (and min-matches max-matches)
      false
      (or min-matches max-matches))))
         

(defn run-pt2
  []
    (println (count (filter true? (map password-is-valid-positionally? puzzle2-real)))))
