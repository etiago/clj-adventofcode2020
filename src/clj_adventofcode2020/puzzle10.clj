(ns clj-adventofcode2020.puzzle10
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn load-puzzle-lines
  [filename]
  (->> (slurp filename)
       (clojure.string/split-lines)
       (map #(Integer/valueOf %))))

(def puzzle10-example
  (->> (load-puzzle-lines "resources/puzzle10-example.txt")))

(def puzzle10-example2
  (->> (load-puzzle-lines "resources/puzzle10-example2.txt")))

(def puzzle10-example3
  (->> (load-puzzle-lines "resources/puzzle10-example3.txt")))

(def puzzle10-real
  (->> (load-puzzle-lines "resources/puzzle10-real.txt")))

(defn run-pt1
  []
  (reduce 
          (fn [counter next-val]

            (let [last-adapter (:last-adapter counter)]
                (case (- next-val last-adapter)
                  1 (merge counter {:one-jmp (inc (:one-jmp counter)) :last-adapter next-val})
                  2 (merge counter {:two-jmp (inc (:two-jmp counter)) :last-adapter next-val})
                  3 (merge counter {:three-jmp (inc (:three-jmp counter)) :last-adapter next-val}))))
          {:one-jmp 0 :two-jmp 0 :three-jmp 1 :last-adapter 0}
          (sort puzzle10-real))
  )

(defn valid-combination?
  [combination device-rating]
  (let [final-count
        (reduce 
         (fn [counter next-val]
           (let [last-adapter (:last-adapter counter)
                 val-difference (- next-val last-adapter)]
             (cond 
               (and (>= val-difference 1)
                    (<= val-difference 3)) (merge counter {:last-adapter next-val})
               :else (merge counter {:is-valid false :last-adapter next-val}))))
         {:is-valid true :last-adapter 0}
         combination)]
    (and (:is-valid final-count)
         (<= (- device-rating (:last-adapter final-count)) 3))))

(defn is-still-valid?
  [state]
  (let [last-adapter (:last-adapter state)
        combination-rest (:combination-rest state)]
    (if (empty? combination-rest)
      state
      (let [val-difference (- (first combination-rest) last-adapter)]
        (if (and (>= val-difference 1)
                 (<= val-difference 3))
          (recur (merge state {:last-adapter (first combination-rest)
                               :combination-rest (rest combination-rest)}))
          false)))))

(defn valid-combination2?
  [combination device-rating]
  (let [final-count (is-still-valid? {:last-adapter 0 :combination-rest combination})]
    (and (map? final-count)
         (<= (- device-rating (:last-adapter final-count)) 3))))

(defn new-good-solutions-for-puzzle
  [puzzle]
  (let [positions-to-check (range 1 (- (count puzzle) 2))]
    (set
     (map
      (fn [pos]
        (let [pos-before (dec pos)
              pos-after (inc pos)
              val-before (nth puzzle pos-before)
              val-after (nth puzzle pos-after)
              val-diff (- val-after val-before)]
          (when (and (<= val-diff 3) (>= val-diff 1))
            (vec (concat (subvec puzzle 0 pos)
                         (subvec puzzle pos-after))))))
      positions-to-check))))

(def new-good-solutions-for-puzzle-memo
  (memoize new-good-solutions-for-puzzle))
  
(defn compute-candidates-to-end
  [state]
  (let [current-puzzle (:current-puzzle state)
        analyzed-solutions (:analyzed-solutions state)
        solutions-to-expand (:solutions-to-expand state)
        new-good-solutions (new-good-solutions-for-puzzle-memo current-puzzle)
        concatted-solutions-to-expand (set/difference (set/union solutions-to-expand new-good-solutions) analyzed-solutions)]
    (println (count analyzed-solutions))
    (if (empty? concatted-solutions-to-expand)
      (merge state {:analyzed-solutions (set/union analyzed-solutions new-good-solutions #{current-puzzle})
                    :solutions-to-expand #{}
                    :current-puzzle nil})
      (let [next-puzzle (first concatted-solutions-to-expand)]
        (recur (merge state
                      {:analyzed-solutions (set/union
                                            analyzed-solutions
                                            #{current-puzzle})
                       :solutions-to-expand (set/difference
                                             concatted-solutions-to-expand
                                             analyzed-solutions
                                             #{next-puzzle})
                       :current-puzzle next-puzzle}))))))

(defn get-indices-n-step
  [puzzle step]
  (let [positions-to-check (range 1 (dec (count puzzle)))]
    (filter
     some?
     (map
      (fn [pos]
        (let [pos-before (dec pos)
              val-before (nth puzzle pos-before)
              val-cur (nth puzzle pos)
              val-diff (- val-cur val-before)]
          (when (= step val-diff)
            pos)))

      positions-to-check))))

(defn all-jumps-less-or-equal-to-3
  [input]
  (every?
   true?
   (map
    #(<= (- (second %) (first %)) 3)
    (partition 2 1 input))))

(defn run-pt2
  []
  (let [current-puzzle (sort puzzle10-real)
        last-el (last current-puzzle)
        device-rating (+ last-el 3)
        current-puzzle-padded (vec (sort (conj (cons 0 current-puzzle) device-rating)))
        indices-three (get-indices-n-step current-puzzle-padded 3)
        values-three (set (map #(nth current-puzzle-padded %) indices-three))
        partitioned (partition-by #(contains? values-three %) current-puzzle-padded)
        possibilities (map #(if (< (dec %) 0)
                              (concat (nth partitioned %) (nth partitioned (inc %)))
                              (if (>= (inc %) (count partitioned))
                                (concat (nth partitioned (dec %)) (nth partitioned %))
                                (concat (nth partitioned (dec %)) (nth partitioned %) (nth partitioned (inc %))))) (range 0 (count partitioned) 2))]
    (time (reduce * (map
     count
     (map
      (fn
        [possibility]
        (let [first-el (first possibility)
             last-el (last possibility)]
          (filter
           true?
           (map #(all-jumps-less-or-equal-to-3 (vec (concat [first-el] % [last-el])))
                (combo/subsets
                 (subvec (vec possibility) 1 (dec (count possibility))))))))
     possibilities))))))

