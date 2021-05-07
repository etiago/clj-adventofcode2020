(ns clj-adventofcode2020.puzzle18
  (:gen-class)

  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :refer (cl-format pprint)]
            [incanter.infix :only (infix-to-prefix formula defop)]
            [incanter.core :only ($=)]))


;; (defn load-puzzle-lines
;;   [filename]
;;   (let [split-lines (map
;;                      str/split-lines
;;                      (str/split
;;                       (slurp filename)
;;                       #"\n\n"))
;;         all-rules-strs (first split-lines)
;;         our-ticket-strs (second split-lines)
;;         other-tickets-strs (third split-lines)
;;         rules-parsed (extract-rules all-rules-strs)
;;         our-ticket (first (parse-tickets (rest our-ticket-strs)))
;;         other-tickets (parse-tickets (rest other-tickets-strs))]
;;     
;;     {:rules rules-parsed
;;      :our-ticket our-ticket
;;      :other-tickets other-tickets}))

;;(defn puzzle16-example
;;  []
;;  (->>
;;   (load-puzzle-lines "resources/puzzle16-example.txt")))

(defn convert [s]
  (try
    (Long/parseLong s)
    (catch NumberFormatException _
      (symbol s))))

(defn expr-arrange [op]
  (fn arrange [expr]
    (cond
      (<= (count expr) 2) expr
      (= (second expr) op)
      (conj (arrange (drop 3 expr))
            (list (second expr) (first expr) (nth expr 2)))
      :else (conj (arrange (rest expr)) (first expr)))))

(def multiply (expr-arrange '*))
(def divide (expr-arrange '/))
(def add (expr-arrange '+))
(def subtract (expr-arrange '-))

(defn infix [expr]
  "Accepts a list containing a valid mathematical expression and returns an evaluatable expression
  conforming to operator prescedence rules."
  (-> expr
      multiply
      divide
      add
      subtract
      add
      first))

(def puzzle18-example
  "1 + 2 * 3 + 4 * 5 + 6")

(incanter.infix/defop '- 10 'incanter.core/minus)
(incanter.infix/defop '+ 10 'incanter.core/plus)
(incanter.infix/defop '/ 10 'incanter.core/div)
(incanter.infix/defop '* 10 'incanter.core/mult)
(incanter.infix/defop '<*> 10 'incanter.core/mmult)
(incanter.infix/defop '<x> 10 'incanter.core/kronecker)
(incanter.infix/defop '** 10 'incanter.core/pow)

(defmacro make-fn [m] 
 `(fn [& args#]
    (eval 
     (cons '~m args#))))

(defn parse-expr
  [expr pos]
  (if (symbol? (nth expr pos))
    (if (and (number? (nth expr (dec pos)))
             (number? (nth expr (inc pos))))
      '~((nth expr pos) (nth expr (dec pos)) (nth expr (inc pos)))
      (if (number? (nth expr (dec pos)))
        '~((nth expr pos) (parse-expr (number? (nth expr (dec pos)))) (nth expr (inc pos)))
        '~((nth expr pos) (nth expr (dec pos)) (parse-expr (number? (nth expr (inc pos)))))))
      (recur expr (inc pos))

    )
      
  )
