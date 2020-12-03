(ns clj-adventofcode2020.puzzle3-test
  (:require [clojure.test :refer :all]
            [clj-adventofcode2020.puzzle3 :as puzzle3]))

(deftest test-puzzle-lines-to-list-of-lists-with-trees-true
  (testing "valid puzzle lines are parsed to valid puzzle board"
    (are [lines puzzle] (= (puzzle3/puzzle-lines-to-list-of-lists-with-trees-true lines) puzzle)
      ["...#" "#.#." "...."]
      [[false false false true] [true false true false] [false false false false]]

      ["...." "...." "...."]
      [[false false false false] [false false false false] [false false false false]])))

(deftest test-is-tree?
  (testing "tree positions correctly return whether position is tree"
    (are [puzzle x y is-tree] (= (puzzle3/is-tree? puzzle x y) is-tree)
      [[false false false]] 0 0 false

      [[false false false]
       [false true false]] 1 1 true

      [[false false false]
       [false false true]] 2 1 true)))

(deftest test-is-outside-to-the-right?
  (testing "returns whether the x is outside of the puzzle to the right"
    (are
        [puzzle x is-outside-to-the-right]
        (= (puzzle3/is-outside-to-the-right? puzzle x) is-outside-to-the-right)
      [[false false false]] 3 true
      [[false false false]] 2 false)))

(deftest test-crossed-bottom?
  (testing "true if y coordinate is below puzzle lines"
    (are
        [puzzle y has-crossed-bottom]
        (= (puzzle3/crossed-bottom? puzzle y) has-crossed-bottom)
      [[false]] 1 true
      [[false]
       [false]] 2 true
      [[false]
       [false]] 1 false
      [[false]
       [false]] 0 false)))

(deftest test-x-or-scaled-back-x-to-fit
  (testing "returns x if within puzzle width"
    (are
        [puzzle x expected-x]
        (= (puzzle3/x-or-scaled-back-x-to-fit puzzle x) expected-x)
      [[false]] 0 0
      [[true true true]] 2 2
      [[true true true]] 1 1))
  (testing "returns a wrapped around x if x goes beyond puzzle width"
    (are
        [puzzle x expected-x]
        (= (puzzle3/x-or-scaled-back-x-to-fit puzzle x) expected-x)
      [[false]] 1 0
      [[true true true]] 3 0
      [[true true true]] 4 1
      [[true true true]] 6 0)))

(deftest test-sliding-coordinates
  (testing "applies sliding delta x and y correctly"
    (is (= (second (puzzle3/sliding-coordinates [[true true true true]] [0 0] [1 2])) [1 2]))
    (is (= (second (puzzle3/sliding-coordinates [[true true true true]] [0 0] [2 1])) [2 1]))
    (is (= (take 3 (puzzle3/sliding-coordinates [[true true true true true]] [0 0] [1 2])) [[0 0] [1 2] [2 4]]))
    (is (= (take 6 (puzzle3/sliding-coordinates [[true true true true true]] [0 0] [1 2])) [[0 0] [1 2] [2 4] [3 6] [4 8] [0 10]]))))
