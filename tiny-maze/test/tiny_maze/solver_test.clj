(ns tiny-maze.solver-test
  (:require [clojure.test :refer :all]
            [tiny-maze.solver :refer :all]))

(defn rand-maze [dims density]
  (let [[xdim ydim] dims]
    (vec (for [x (range xdim)]
           (vec (for [y (range ydim)]
                  (if (> (rand) density) 0 1)))))))

(deftest test-solve-maze
  (testing "can find way to exit with 3x3 maze"
    (let [maze [[:S 0 1]
                [1  0 1]
                [1  0 :E]]
          sol [[:x :x 1]
               [1  :x 1]
               [1  :x :x]]]
      (is (= sol (solve-maze maze)))))

  (testing "can find way to exit with 4x4 maze"
    (let [maze [[:S 0 0 1]
                [1  1 0 0]
                [1  0  0 1]
                [1  1  0 :E]]
          sol [[:x :x :x 1]
               [1  1 :x 0]
               [1  0 :x 1]
               [1  1  :x :x]]]
      (is (= sol (solve-maze maze))))))

(deftest test-solve-shortest
  (testing "uses shortest path with 8x8 maze"
    (let [maze [[:S 0 0 1 1 1 1 0]
                [0 0 0 0 0 0 0 0]
                [0 0 1 0 1 0 0 0]
                [0 0 0 0 1 0 0 1]
                [0 0 0 0 1 1 0 0]
                [0 1 0 0 0 0 0 0]
                [0 1 0 0 0 0 1 1]
                [0 0 0 0 1 0 0 :E]]
          sol (solve-maze maze)
          shortest-path-len 15]
      (is (= shortest-path-len
             (->> sol
                  (reduce concat)
                  (filter #(= % :x))
                  (count))
             ))
      )))
