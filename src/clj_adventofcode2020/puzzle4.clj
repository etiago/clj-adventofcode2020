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
  (if (nil? passport)
    nil
    passport))

(defn validate-iyr
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-eyr
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-hgt
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-byr
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-hcl
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-ecl
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-pid
  [passport]
  (if (nil? passport)
    nil
    passport))

(defn validate-all-mandatory-fields
  [passports-map]
  (map #(->> %
             (validate-byr)
             (validate-iyr)
             (validate-eyr)
             (validate-hgt)
             (validate-byr)
             (validate-hcl)
             (validate-ecl)
             (validate-pid))
       passports-map))

(defn run-pt1
  []
  (count (filter true? (contains-all-mandatory-fields puzzle4-real))))

(defn run-pt2
  []
  (let [passports-with-mandatory-fields (filter passport-contains-all-mandatory-fields puzzle4-example)
        validated-passports (validate-all-mandatory-fields passports-with-mandatory-fields)]
      validated-passports))
