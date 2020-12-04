(ns clj-adventofcode2020.puzzle4
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn tuple-str-to-map
  [tuple]
  {(keyword (first tuple)) (second tuple)})

(defn load-puzzle-file
  [filename]
  (as-> (slurp filename) f
    (clojure.string/split f #"\n\n")
    (map #(clojure.string/replace % #"\n" " ") f)
    (map (fn
           [line]
           (->> (clojure.string/split line #" ")
                (map (fn
                       [segment]
                       (->> (clojure.string/split segment #":")
                            (tuple-str-to-map)))))) f)
    (map (fn [list-of-maps] (into {} list-of-maps)) f)))

(def puzzle4-example
  (load-puzzle-file "resources/puzzle4-example.txt"))

(def puzzle4-pt2-example
  (load-puzzle-file "resources/puzzle4-pt2-example.txt"))

(def puzzle4-real
  (load-puzzle-file "resources/puzzle4-real.txt"))

(defn passport-contains-all-mandatory-fields
  [passport]
  (let [mandatory-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid}
        mandatory-field-count (count mandatory-fields)]
    (= mandatory-field-count
       (count (set/intersection mandatory-fields (set (keys passport)))))))

(defn contains-all-mandatory-fields
  [passports-map]
  (map
   passport-contains-all-mandatory-fields
   passports-map))

(defn validate-byr
  [passport]
  (when (some? passport)
    (try
      (let [byr-parsed (Integer/valueOf (get passport :byr))]
        (when (and (>= byr-parsed 1920)
                   (<= byr-parsed 2002))
          passport))
      (catch Exception e nil))))

(defn validate-iyr
  [passport]
  (when (some? passport)
    (try
      (let [iyr-parsed (Integer/valueOf (get passport :iyr))]
        (when (and (>= iyr-parsed 2010)
                   (<= iyr-parsed 2020))
          passport))
      (catch Exception e nil))))

(defn validate-eyr
  [passport]
  (when (some? passport)
    (try
      (let [eyr-parsed (Integer/valueOf (get passport :eyr))]
        (when (and (>= eyr-parsed 2020)
                   (<= eyr-parsed 2030))
          passport))
      (catch Exception e nil))))

(defn validate-hgt-cm
  [passport]
  (try
    (let [hgt-parsed (Integer/valueOf (re-find #"\d+" (get passport :hgt)))]
      (when (and (>= hgt-parsed 150)
                 (<= hgt-parsed 193))
        passport))
    (catch Exception e nil)))

(defn validate-hgt-in
  [passport]
(try
      (let [hgt-parsed (Integer/valueOf (re-find #"\d+" (get passport :hgt)))]
        (if (and (>= hgt-parsed 59)
                 (<= hgt-parsed 76))
          passport
          nil))
      (catch Exception e nil))
  )

(defn validate-hgt
  [passport]
  (when (some? passport)
    (let [hgt (get passport :hgt)]
      (cond
        (str/includes? hgt "in") (validate-hgt-in passport)
        (str/includes? hgt "cm") (validate-hgt-cm passport)
        :else nil))))

(defn validate-hcl
  [passport]
  (when
      (and
       (some? passport)
       (some? (re-matches #"#[a-f0-9]{6}" (get passport :hcl))))
    passport))

(defn validate-ecl
  [passport]
  (when
      (and
       (some? passport)
       (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (get passport :ecl)))
    passport))

(defn validate-pid
  [passport]
  (when
      (and
       (some? passport)
       (some? (re-matches #"[0-9]{9}" (get passport :pid))))
    passport))

(defn validate-all-mandatory-fields
  [passports]
  (map #(->> %
             (validate-byr)
             (validate-iyr)
             (validate-eyr)
             (validate-hgt)
             (validate-byr)
             (validate-hcl)
             (validate-ecl)
             (validate-pid))
       passports))

(defn run-pt1
  []
  (count (filter true? (contains-all-mandatory-fields puzzle4-real))))

(defn run-pt2
  []
  (let [passports-with-mandatory-fields (filter passport-contains-all-mandatory-fields puzzle4-real)
        validated-passports (validate-all-mandatory-fields passports-with-mandatory-fields)]
      (count (filter identity validated-passports))))
