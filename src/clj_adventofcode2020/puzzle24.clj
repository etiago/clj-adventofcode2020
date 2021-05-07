(ns clj-adventofcode2020.puzzle24
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]))

(defn load-puzzle-lines
  [filename]
  (-> (slurp filename)
      (str/split-lines)))

(defn puzzle24-example
  []
  (load-puzzle-lines "resources/puzzle24-example.txt"))

(defn puzzle24-real
  []
  (load-puzzle-lines "resources/puzzle24-real.txt"))

(defn get-next-move
  [rest-str]
  (cond
    (str/starts-with? rest-str "nw") {:move :nw :rest-str (subs rest-str 2)}
    (str/starts-with? rest-str "sw") {:move :sw :rest-str (subs rest-str 2)}
    (str/starts-with? rest-str "w") {:move :w :rest-str (subs rest-str 1)}
    (str/starts-with? rest-str "ne") {:move :ne :rest-str (subs rest-str 2)}
    (str/starts-with? rest-str "se") {:move :se :rest-str (subs rest-str 2)}
    (str/starts-with? rest-str "e") {:move :e :rest-str (subs rest-str 1)}))

(defn extract-moves
  [str moves]
  (let [next-moves (get-next-move str)
        rest-str (:rest-str next-moves)]
    (if (empty? rest-str)
      (conj moves (:move next-moves))
      (recur rest-str  (conj moves (:move next-moves))))))

(def move-to-fn
  {:nw (fn [position] {:x (- (:x position) 0.5)
                       :y (+ (:y position) 1.0)})
   :w (fn [position] {:x (- (:x position) 1.0)
                      :y (:y position)})
   :sw (fn [position] {:x (- (:x position) 0.5)
                       :y (- (:y position) 1.0)})
   
   :ne (fn [position] {:x (+ (:x position) 0.5)
                       :y (+ (:y position) 1)})
   :e (fn [position] {:x (+ (:x position) 1.0)
                      :y (:y position)})
   :se (fn [position] {:x (+ (:x position) 0.5)
                       :y (- (:y position) 1.0)})})

(defn apply-moves
  ([moves]
   (apply-moves moves {:x 0.0 :y 0.0}))
  ([moves position]
   (if (not-empty moves)
     (recur (rest moves) ((get move-to-fn (first moves)) position))
     position)))

(defn flip-tile
  [position tiles]
  (if (= :white (get tiles position :white))
    (assoc tiles position :black)
    (assoc tiles position :white)))

(defn run-pt1
  []
  (let [current-puzzle (puzzle24-real)
        move-list (map #(extract-moves % []) current-puzzle)]

    (reduce
     #(let [new-position (apply-moves %2)
            new-tiles (flip-tile new-position (:tiles %1))]
        {:tiles new-tiles})
     {:tiles {}}
     move-list)))


(defn count-black-neighbors
  [board position]
  (let [neighbors (map #(% position) (vals move-to-fn))
        neighbors-vals (map #(get board % :white) neighbors)]
    (count (filter #(= :black %) neighbors-vals))))

(defn get-neighbor-coordinates-for-position
  [position]
  (map #(% position) (vals move-to-fn)))

(defn pad-board-blacks-with-whites
  [current-board]
  (let [black-positions (keys (filter #(= :black (second %)) current-board))
        black-surroundings (flatten (map
                                     get-neighbor-coordinates-for-position
                                     black-positions))]
    (merge
     current-board
     (into {}
           (map
            #(identity [% (get current-board % :white)])
            black-surroundings)))))

(defn compute-new-board
  [current-board]
  (map
   #(let [black-neighbors (count-black-neighbors current-board (first %))]
      (cond
        (and (= (second %) :black)
             (or (= 0 black-neighbors)
                 (> black-neighbors 2))) [(first %) :white]
        (and (= (second %) :white)
             (= 2 black-neighbors)) [(first %) :black]
        :else %))
   current-board)
  )

(defn run-pt2
  []
  (let [total-runs 100
        initial-board (run-pt1)
        initial-tiles (:tiles initial-board)
        final-board (loop [tiles initial-tiles
                           runs 0]
                      (if (= runs total-runs)
                        tiles
                        (recur (into {}
                                     (compute-new-board
                                      (pad-board-blacks-with-whites tiles)))
                               (inc runs))))]
    (count (filter #(= % :black) (vals final-board)))))
