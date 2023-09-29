(ns magic-square.puzzle-test
  (:require [clojure.test :refer :all]
            [magic-square.puzzle :refer :all]))

(defn sum-rows [m]
  (map #(reduce + %) m))

(defn sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(deftest test-magic-square
  (testing "all the rows, columns, and diagonal add to the same number"
    (let [sqr (magic-square values)]
      (is (= (set (sum-rows sqr))
             (set (sum-cols sqr))
             (set (sum-diagonals sqr))))
      (is (= 1
             (count (set (sum-rows sqr)))
             (count (set (sum-cols sqr)))
             (count (set (sum-diagonals sqr))))))))
