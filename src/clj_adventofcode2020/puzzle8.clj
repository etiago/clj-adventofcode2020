(ns clj-adventofcode2020.puzzle8
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def fn-map
  {"+" +
   "-" -})

(defn jmp
  [state signal-fn operand]
  (let [{next-operation-idx :next-operation-idx
         visited-positions :visited-positions
         accumulator :accumulator} state]  
    (merge state
           {:next-operation-idx (signal-fn next-operation-idx operand)
            :visited-positions (set/union #{next-operation-idx} visited-positions)
            :accumulator accumulator})))

(defn nop
  [state signal-fn operand]
  (let [{next-operation-idx :next-operation-idx
         visited-positions :visited-positions
         accumulator :accumulator} state]
    (merge state
           {:next-operation-idx (inc next-operation-idx)
            :visited-positions (set/union #{next-operation-idx} visited-positions)
            :accumulator accumulator})))

(defn acc
  [state signal-fn operand]
  (let [{next-operation-idx :next-operation-idx
         visited-positions :visited-positions
         accumulator :accumulator} state]
    (merge state
           {:next-operation-idx (inc next-operation-idx)
            :visited-positions (set/union #{next-operation-idx} visited-positions)
            :accumulator (signal-fn accumulator operand)})))

(def instruction-map
  {"jmp" jmp
   "nop" nop
   "acc" acc})

(defn parse-code-line
  [code-line]
  (let [[instruction-str delta] (str/split code-line #" ")
        instruction-fn (get instruction-map instruction-str)
        signal (subs delta 0 1)
        signal-fn (get fn-map signal)
        operand (Integer/parseInt (subs delta 1))]
    {:instruction-fn instruction-fn
     :orig-str code-line
     :signal-fn signal-fn
     :operand operand}))

(defn load-puzzle-lines
  [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)))

(defn generate-new-puzzles
  [puzzle-lines cur-pos new-puzzles]
  (if (>= cur-pos (count puzzle-lines))
    new-puzzles
    (let [current-instruction (nth puzzle-lines cur-pos)
          sign-and-operand (second (str/split current-instruction #" "))]
      (if (str/includes? current-instruction "jmp")
        (recur puzzle-lines
               (inc cur-pos)
               (into new-puzzles [(assoc puzzle-lines cur-pos (str "nop " sign-and-operand))]))
        (recur puzzle-lines
               (inc cur-pos)
               new-puzzles)))))

(defn load-puzzle-file
  [filename]
  (->> (slurp filename)
      (clojure.string/split-lines)
      (map parse-code-line)))

(def puzzle8-example-lines
  (->> (load-puzzle-lines "resources/puzzle8-example.txt")))

(def puzzle8-example
  (->> (load-puzzle-file "resources/puzzle8-example.txt")))

(def puzzle8-real-lines
  (->> (load-puzzle-lines "resources/puzzle8-real.txt")))

(def puzzle8-real
  (->> (load-puzzle-file "resources/puzzle8-real.txt")))

(defn puzzle-to-accumulator-at-stop
  [instruction-list initial-state]
  (loop [state initial-state]
    (let [{next-operation-idx :next-operation-idx
           visited-positions :visited-positions
           accumulator :accumulator} state]
      (if (< next-operation-idx (count instruction-list))
        (if (contains? visited-positions next-operation-idx)
          state
          (let [instruction (nth instruction-list next-operation-idx)
                {instruction-fn :instruction-fn
                 signal-fn :signal-fn
                 operand :operand} instruction]
            (recur (instruction-fn state signal-fn operand))))
        (assoc state :stopped true)))))

(defn run-pt1
  []
  (let [current-puzzle puzzle8-real]
    (puzzle-to-accumulator-at-stop
     current-puzzle
     {:next-operation-idx 0
      :visited-positions #{}
      :accumulator 0
      :stopped false})))

(defn run-pt2
  []
  (let [current-puzzle puzzle8-real-lines
        extra-puzzles (generate-new-puzzles current-puzzle 0 [])
        all-puzzles-str (concat extra-puzzles [current-puzzle])
        all-puzzles (map #(map parse-code-line %) all-puzzles-str)]
    (some #(if (get % :stopped) %)
          (map #(puzzle-to-accumulator-at-stop % {:next-operation-idx 0
                                                  :visited-positions #{}
                                                  :accumulator 0
                                                  :stopped false}) all-puzzles))))

