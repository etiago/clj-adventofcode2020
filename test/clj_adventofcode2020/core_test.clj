(ns clj-adventofcode2020.core-test
  (:require [clojure.test :refer :all]
            [clj-adventofcode2020.core :refer :all]
            [clj-adventofcode2020.puzzle2 :as puzzle2]))

(deftest test-puzzle-line-to-puzzle-and-password
  (testing "properly formatted lines are parsed correctly to valid maps"
    (are [line map] (= (puzzle2/puzzle-line-to-policy-and-password line) map)
      "1-3 a: abcde" {:policy {:min 1 :max 3 :letter \a} :password "abcde"}
      "1-3 b: cdefg" {:policy {:min 1 :max 3 :letter \b} :password "cdefg"}
      "2-9 c: ccccccccc" {:policy {:min 2 :max 9 :letter \c} :password "ccccccccc"})))

(deftest test-password-is-valid?
  (testing "passwords with no less than min and no more than max are accepted, otherwise rejected"
    (are [accepted policy-and-password] (= accepted (puzzle2/password-is-valid? policy-and-password))
      true {:policy {:min 1 :max 3 :letter \a} :password "abcde"}
      false {:policy {:min 1 :max 3 :letter \b} :password "cdefg"}
      true {:policy {:min 2 :max 9 :letter \c} :password "ccccccccc"})))

(deftest test-password-is-valid-positionally?
  (testing "passwords with exactly one correct character positionally on positions min or max are accepted, otherwise rejected"
    (are [accepted policy-and-password] (= accepted (puzzle2/password-is-valid-positionally? policy-and-password))
      true {:policy {:min 1 :max 3 :letter \a} :password "abcde"}
      false {:policy {:min 1 :max 3 :letter \b} :password "cdefg"}
      false {:policy {:min 2 :max 9 :letter \c} :password "ccccccccc"})))

(deftest test-run-pt1
  (testing "running part one of the puzzle returns the correct answer"
    (is (= 548 (puzzle2/run-pt1)))))

(deftest test-run-pt2
  (testing "running part two of the puzzle returns the correct answer"
    (is (= 502 (puzzle2/run-pt2)))))
