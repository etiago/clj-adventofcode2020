(ns clj-adventofcode2020.puzzle11
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-puzzle-lines
  [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)
       (map #(str/split % #""))
       ))

(def puzzle11-example
  (->>
   (load-puzzle-lines "resources/puzzle11-example.txt")
   
   ))

(def puzzle11-real
  (->>
   (load-puzzle-lines "resources/puzzle11-real.txt")
   
   ))

(defn puzzle-lines-to-empty-chairs
  [puzzle-lines]
  (mapcat identity
          (map-indexed
           (fn [y row]
             (remove
              nil?
              (map-indexed
               (fn [x val]
                 (when (= val "L")
                   [x y]))
               row))) puzzle-lines)))

(def surrounding-mask
  [[-1 -1] [0 -1] [1 -1]
   [-1 0] [1 0]
   [-1 1] [0 1] [1 1]])

(defn possible-surrounding-positions
  [position width height]
  (let [x (first position)
        y (second position)]
    (filter
     (fn [masked-position]
        (and (< (first masked-position) width)
             (>= (first masked-position) 0)
             (< (second masked-position) height)
             (>= (second masked-position) 0)
             (not= masked-position position)))
     (map #(identity [(+ x (first %)) (+ y (second %))]) surrounding-mask))))

(defn count-occupied-adjacent
  [state position]
  (let [{width :width
         height :height
         seating :seating} state
        adjacent (possible-surrounding-positions position width height)]
    (count (filter true? (map #(= "#" (get seating %)) adjacent)))))

(defn get-next-state
  [current-state]
  (let [{width :width
         height :height
         seating :seating} current-state
        positions (keys seating)]
    (loop [positions-to-check positions
           new-seating seating]
      (if (empty? positions-to-check)
        (merge current-state {:seating new-seating})
        (let [position (first positions-to-check)
              occupied-adjacent (count-occupied-adjacent current-state position)
              current-seat (get seating position)]
          (if (= "L" current-seat)
            (if (= 0 occupied-adjacent)
              (recur (rest positions-to-check) (merge new-seating {position "#"}))
              (recur (rest positions-to-check) (merge new-seating {position "L"})))
            (when (= "#" current-seat)
              (if (>= occupied-adjacent 4)
                (recur (rest positions-to-check) (merge new-seating {position "L"}))
                (recur (rest positions-to-check) (merge new-seating {position "#"}))))))))))
      
(defn pretty-print-board
  [state]
  (let [{width :width
         height :height
         seating :seating} state]
    (map (fn [y]
           (reduce #(str %1 %2)
                   (map (fn [x]
                          (get seating [x y] "."))
                        (range 0 width)))) (range 0 height))))

(defn get-stable-state
  [previous-state new-state]
  (if (= previous-state new-state)
    previous-state
    (recur new-state (get-next-state new-state))))

(defn count-occuppied
  [state]
  (count (filter #(= "#" %) (vals (:seating state)))))

(defn run-pt1
  []
  (let [current-puzzle-lines puzzle11-real
        height (count current-puzzle-lines)
        width (count (first current-puzzle-lines))
        empty-chair-positions (set (puzzle-lines-to-empty-chairs current-puzzle-lines))
        state {:seating (zipmap empty-chair-positions (repeat "L")) :width width :height height}]
    (count-occuppied (get-stable-state state (get-next-state state)))


    ))

(defn add-b-to-a
  [pos-a pos-b]
  [(+ (first pos-a) (first pos-b)) (+ (second pos-a) (second pos-b))])


(defn get-expanded-surrounding-positions
  []
  (map
   #(take 5 (reductions add-b-to-a (repeat %)))
   surrounding-mask))

(defn position-is-outside?
  [state position]
  (let [{width :width
         height :height} state]
    (or (< (first position) 0)
        (>= (first position) width)
        (< (second position) 0)
        (>= (second position) height))))

(defn position-is-occuppied?
  [state position]
  (= "#" (get (:seating state) position ".")))

(defn position-is-free-seat?
  [state position]
  (= "L" (get (:seating state) position ".")))

(defn take-until-outside-or-occuppied-or-free
  [state masks position]
  (let [outside-or-occupied
        (filter
         #(do
            (or (position-is-outside? state %)
                (position-is-occuppied? state %)
                (position-is-free-seat? state %)))
         (map
          #(add-b-to-a position %)
          masks))
        last-checked (first outside-or-occupied)]
    (cond
      (position-is-occuppied? state last-checked) {:seat last-checked :status :occupied}
      (position-is-free-seat? state last-checked) {:seat last-checked :status :free}
      :else {:seat last-checked :status :outside})))

(defn count-occuppied-adjacent-new-rules
  [state position]
  (let [all-mask-expansions (map #(reductions add-b-to-a (repeat %)) surrounding-mask)]
    (group-by
     :status
     (map
      (fn [mask-expansion]
        (take-until-outside-or-occuppied-or-free state mask-expansion position))
      all-mask-expansions))))

(defn get-next-state-new-rules
  [current-state]
  (let [{width :width
         height :height
         seating :seating} current-state
        positions (keys seating)]
    (loop [positions-to-check positions
           new-seating seating]
      (if (empty? positions-to-check)
        (merge current-state {:seating new-seating})
        (let [position (first positions-to-check)
              occupied-or-free-adjacent (count-occuppied-adjacent-new-rules current-state position)
              current-seat (get seating position)]
          (if (= "L" current-seat)
            (if (= 0 (count (:occupied occupied-or-free-adjacent)))
              (recur (rest positions-to-check) (merge new-seating {position "#"}))
              (recur (rest positions-to-check) (merge new-seating {position "L"})))
            (when (= "#" current-seat)
              (if (>= (count (:occupied occupied-or-free-adjacent)) 5)
                (recur (rest positions-to-check) (merge new-seating {position "L"}))
                (recur (rest positions-to-check) (merge new-seating {position "#"}))))))))))

(defn get-stable-state-new-rules
  [previous-state new-state]
  (if (= previous-state new-state)
    previous-state
    (recur new-state (get-next-state-new-rules new-state))))

(defn run-pt2
  []
  (let [current-puzzle-lines puzzle11-real
        height (count current-puzzle-lines)
        width (count (first current-puzzle-lines))
        empty-chair-positions (set (puzzle-lines-to-empty-chairs current-puzzle-lines))
        state {:seating (zipmap empty-chair-positions (repeat "L")) :width width :height height}
        final-state (get-stable-state-new-rules state (get-next-state-new-rules state))]


    ;;(pretty-print-board final-state)

     (count (filter #(= % "#") (vals (:seating final-state))))
  
  ))
