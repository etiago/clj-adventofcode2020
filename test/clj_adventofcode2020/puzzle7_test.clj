(ns clj-adventofcode2020.puzzle7-test
  (:require [clojure.test :refer :all]
            [clj-adventofcode2020.puzzle7 :as puzzle7]))

(deftest test-children-to-repeated-list
  (testing "map of bag to quantity correctly gives bag repeated list of bags"
    (is (= '(:foo :foo :foo :foo :foo :bar :bar :bar :bar) (puzzle7/children-to-repeated-list {:foo 5 :bar 4}))))
  (testing "empty map returns empty list"
    (is (= '() (puzzle7/children-to-repeated-list {})))))
