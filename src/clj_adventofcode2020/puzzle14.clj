(ns clj-adventofcode2020.puzzle14
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]))

(defn load-puzzle-lines
  [filename]
  (let [split-lines (str/split-lines
                     (slurp filename))]
    split-lines))

(defn extract-mask
  [line]
  (let [mask-str (nth (str/split line #" ") 2)]
    (reduce
     merge
    (filter
     some?
     (map-indexed
     #(when-not (= %2 \X)
        {%1 %2})
    (reverse mask-str))))))

(defn extract-mem-writes
  [line]
  (let [split-spaces (str/split line #" ")]
    {:position (Integer/parseInt (second (str/split (first split-spaces) #"[\[\]]")))
     :value (Integer/parseInt (nth split-spaces 2))}))

(defn puzzle14-example
  []
  (->>
   (load-puzzle-lines "resources/puzzle14-example.txt")))

(defn puzzle14-example2
  []
  (->>
   (load-puzzle-lines "resources/puzzle14-example2.txt")))

(defn puzzle14-real
  []
  (->>
   (load-puzzle-lines "resources/puzzle14-real.txt")))

(defn apply-mask
  [value mask]
  (let [
        minimum-size (inc (apply max (keys mask)))
        value-chars (map #(Character/digit % 10) (Integer/toString value 2))
        reversed-value-chars (reverse value-chars)
        padding-needed (- minimum-size (count reversed-value-chars))
        reversed-value-chars-with-padding (concat
                                           reversed-value-chars
                                           (repeat padding-needed 0))
        expanded-mask (flatten (into [] mask))]
    (BigInteger.
     (str/join
      (drop-while #(= % 0)
                  (reverse
                   (apply
                    assoc
                    (concat [(vec reversed-value-chars-with-padding)] expanded-mask))))) 2)))

(defn all-indices-of
  [s c]
  (map
   first
   (filter
    #(= c (second %))
    (map-indexed
     #(identity [%1 %2])
     s))))

(defn extract-mask-expanded
  [line]
  (let [reversed-mask-str (reverse (nth (str/split line #" ") 2))
        xs (all-indices-of reversed-mask-str \X)
        ones (all-indices-of reversed-mask-str \1)
        selections (combo/selections [0 1] (count xs))]
    (map
     #(merge (zipmap xs %) (zipmap ones (repeat 1)))
     selections)))

;;
(defn run-pt1
  []
  (let [lines (puzzle14-real)
        final-state (reduce
                     (fn [state line]
                       (cond
                         (str/includes? line "mask") (merge state {:mask (extract-mask line)})
                         (str/includes? line "mem") (let [mem-write (extract-mem-writes line)
                                                          val-with-mask (apply-mask (:value mem-write) (:mask state))]
                                                      (merge
                                                       state
                                                       {:memory (set/union
                                                                 (:memory state)
                                                                 {(:position mem-write)
                                                                  val-with-mask})}))))
                     {:mask {} :memory {}}      
                     lines)]
    (apply + (vals (:memory final-state)))))

(defn memory-changer
  [masks position value]
  (fn [memory]
    (merge
     memory
     (reduce
      #(merge %1 {(apply-mask position %2) value})
      {}
      masks))))

(defn line-reducer
  [state line]
  (cond
    (str/includes? line "mask") (merge state {:masks (extract-mask-expanded line)})
    (str/includes? line "mem") (let [mem-write (extract-mem-writes line)]

                                      (update-in
                                       state
                                       [:memory]
                                       (memory-changer
                                        (:masks state) (:position mem-write) (:value mem-write))))))
  
(defn run-pt2
  []
  (let [lines (puzzle14-real)
        final-state (reduce line-reducer {:masks {} :memory {}} lines)]
    (reduce + (vals (:memory final-state)))))
