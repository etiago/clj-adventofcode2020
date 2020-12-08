(ns clj-adventofcode2020.puzzle8
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def fn-map
  {"+" +
   "-" -})

(defn parse-operation-str
  [operation-str]
  (let [split-operation-str (str/split operation-str #" ")
        signal (subs (second split-operation-str) 0 1)
        signal-fn (get fn-map signal)
        operand (Integer/parseInt (subs (second split-operation-str) 1))
        applied-signal-fn (fn [other-operand] (signal-fn other-operand operand))]
    
    {:operation (first split-operation-str)
     :orig-str operation-str
     :fn applied-signal-fn}))

(defn load-puzzle-lines
  [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)))

(defn generate-new-puzzles
  [puzzle-lines cur-pos new-puzzles]
  (if (>= cur-pos (count puzzle-lines))
    new-puzzles
    (if (str/includes? (nth puzzle-lines cur-pos) "jmp")
      (let [split-op (str/split (nth puzzle-lines cur-pos) #" ")]
        (recur puzzle-lines (inc cur-pos) (into new-puzzles [(assoc puzzle-lines cur-pos (str "nop " (second split-op)))])))
      (recur puzzle-lines (inc cur-pos) new-puzzles))))

(defn load-puzzle-file
  [filename]
  (->> (slurp filename)
      (clojure.string/split-lines)
      (map parse-operation-str)))

(def puzzle8-example-lines
  (->> (load-puzzle-lines "resources/puzzle8-example.txt")))

(def puzzle8-example
  (->> (load-puzzle-file "resources/puzzle8-example.txt")))

(def puzzle8-real-lines
  (->> (load-puzzle-lines "resources/puzzle8-real.txt")))

(def puzzle8-real
  (->> (load-puzzle-file "resources/puzzle8-real.txt")))

(defn puzzle-to-accumulator-at-stop
  [current-puzzle]
  (loop [next-operation-idx 0
         visited-positions #{}
         accumulator 0]
      (if (< next-operation-idx (count current-puzzle))
        (if (contains? visited-positions next-operation-idx)
          {:stop false :accumulator accumulator}
          (let [op (nth current-puzzle next-operation-idx)
                op-str (get op :operation)
                op-fn (get op :fn)]
            (cond
              (= "acc" op-str) (recur (inc next-operation-idx)
                                      (set/union #{next-operation-idx} visited-positions)
                                      (op-fn accumulator))
              (= "jmp" op-str) (recur (op-fn next-operation-idx)
                                      (set/union #{next-operation-idx} visited-positions)
                                      accumulator)
              (= "nop" op-str) (recur (inc next-operation-idx)
                                      (set/union #{next-operation-idx} visited-positions)
                                      accumulator))))
        {:stop true :accumulator accumulator})))

(defn run-pt1
  []
  (let [current-puzzle puzzle8-real]
    (puzzle-to-accumulator-at-stop current-puzzle)))

(defn run-pt2
  []
  (let [current-puzzle puzzle8-real-lines
        extra-puzzles (generate-new-puzzles current-puzzle 0 [])
        all-puzzles-str (concat extra-puzzles [current-puzzle])
        all-puzzles (map #(map parse-operation-str %) all-puzzles-str)]
    (some #(if (get % :stop) %) (map puzzle-to-accumulator-at-stop all-puzzles))))

