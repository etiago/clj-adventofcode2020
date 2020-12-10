(ns clj-adventofcode2020.puzzle4-test
  (:require [clojure.test :refer :all]
            [clj-adventofcode2020.puzzle4 :as puzzle4]))

(deftest test-passport-contains-all-fields
  (testing "correctly returns true if passport has at least all fields"
    (is (= true (puzzle4/passport-contains-all-fields {:foo 123 :bar 456} #{:foo :bar})))
    (is (= true (puzzle4/passport-contains-all-fields {:foo 123 :bar 456} #{:foo}))))
  (testing "correctly returns false if passport missing at least one field"
    (is (= false (puzzle4/passport-contains-all-fields {:foo 123 :bar 456} #{:baz})))
    (is (= false (puzzle4/passport-contains-all-fields {:foo 123 :bar 456} #{:baz :bax})))))

(deftest test-validate-numeric-field
  (testing "correctly validates number when number is in range"
    (are [passport field min-val max-val] (= (puzzle4/validate-numeric-field passport field min-val max-val) passport)
      {:foo "10"} :foo 5 15
      {:foo "5"} :foo 5 15
      {:foo "15"} :foo 5 15))
  (testing "returns nil if number is invalid"
    (are [passport field min-val max-val] (nil? (puzzle4/validate-numeric-field passport field min-val max-val))
      {:foo "abc"} :foo 5 15
      {:foo "5a"} :foo 5 15))
  (testing "returns nil if number is outside of range"
    (are [passport field min-val max-val] (nil? (puzzle4/validate-numeric-field passport field min-val max-val))
      {:foo "16"} :foo 5 15
      {:foo "4"} :foo 5 15)))
