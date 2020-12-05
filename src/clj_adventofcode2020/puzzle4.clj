(ns clj-adventofcode2020.puzzle4
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn tuple-str-to-map
  [tuple]
  {(keyword (first tuple)) (second tuple)})

(defn line-to-passport-map
  [line]
  (->> (clojure.string/split line #" ")
       (map (fn
              [segment]
              (->> (clojure.string/split segment #":")
                   (tuple-str-to-map))))
       (into {})))

(defn load-puzzle-file
  [filename]
  (-> (slurp filename)
      (clojure.string/split #"\n\n")
      (as-> f (map #(clojure.string/replace % #"\n" " ") f))
      (as-> f (map line-to-passport-map f))))

(def puzzle4-example
  (load-puzzle-file "resources/puzzle4-example.txt"))

(def puzzle4-pt2-example
  (load-puzzle-file "resources/puzzle4-pt2-example.txt"))

(def puzzle4-real
  (load-puzzle-file "resources/puzzle4-real.txt"))

(defn passport-contains-all-fields
  [passport fields]
  (let [field-count (count fields)]
    (= field-count
       (count (set/intersection fields (set (keys passport)))))))

(defn all-passports-contain-all-fields
  [passports-map fields]
  (map
   #(passport-contains-all-fields % fields)
   passports-map))

(defn validate-numeric-field
  [passport field min-value-inclusive max-value-inclusive]
  (when (some? passport)
    (try
      (let [parsed (Integer/valueOf (get passport field))]
        (when (and (>= parsed min-value-inclusive)
                   (<= parsed max-value-inclusive))
          passport))
      (catch Exception e nil))))

(defn validate-hgt-min-max
  [passport field min-max]
  (try
    (let [hgt-parsed (Integer/valueOf (re-find #"\d+" (get passport field)))]
      (when (and (>= hgt-parsed (get min-max :min))
                 (<= hgt-parsed (get min-max :max)))
        passport))
    (catch Exception e nil)))

(defn validate-hgt
  [passport field limits-map]
  (when (some? passport)
    (let [hgt (get passport :hgt)]
      (cond
        (str/includes? hgt "in") (validate-hgt-min-max passport field (get limits-map "in"))
        (str/includes? hgt "cm") (validate-hgt-min-max passport field (get limits-map "cm"))
        :else nil))))

(defn validate-field-is-in-set
  [passport field possible-set]
  (when
      (and
       (some? passport)
       (contains? possible-set (get passport field)))
    passport))

(defn validate-field-with-regex
  [passport field pattern]
  (when
      (and
       (some? passport)
       (some? (re-matches pattern (get passport field))))
    passport))

(defn validate-all-mandatory-fields
  [passports]
  (map #(-> %
            (validate-numeric-field :byr 1920 2002)
            (validate-numeric-field :iyr 2010 2020)
            (validate-numeric-field :eyr 2020 2030)
            (validate-hgt :hgt {"in" { :min 59 :max 76 }
                                "cm" { :min 150 :max 193 }})
            (validate-field-with-regex :hcl #"#[a-f0-9]{6}")
            (validate-field-is-in-set :ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
            (validate-field-with-regex :pid #"[0-9]{9}"))
       passports))

(defn run-pt1
  []
  (count (filter true? (all-passports-contain-all-fields puzzle4-real #{:byr :iyr :eyr :hgt :hcl :ecl :pid}))))

(defn run-pt2
  []
  (->> puzzle4-real
       (filter #(passport-contains-all-fields % #{:byr :iyr :eyr :hgt :hcl :ecl :pid}))
       (validate-all-mandatory-fields)
       (filter some?)
       (count)))
