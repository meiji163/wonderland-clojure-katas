(ns magic-square.puzzle
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])
(def sqr-size 3)

(defn sumo [l sum]
  "constrain sum of list l to equal sum"
  (fresh [a d sum-rest]
    (conde
     [(== l ()) (== sum 0)]
     [(conso a d l)
      (fd/+ a sum-rest sum)
      (sumo d sum-rest)])))


(defn run-solve [vals]
  (let [vars (repeatedly (* sqr-size sqr-size) lvar)
        rows (->> vars
                  (partition sqr-size)
                  (map vec)
                  (into []))
        cols (apply map vector rows)
        diag (map #(get-in rows [% %])
                  (range 0 sqr-size))
        anti-diag (map #(get-in rows [% (dec (- sqr-size %))])
                       (range 0 sqr-size))

        ;; range of magic numbers to search through
        min-sum 16
        max-sum 20]
    (run 1 [q sum]
      (== q vars)
      (fd/distinct vars)
      (fd/in sum (fd/interval min-sum max-sum))
      (everyg #(membero % vals) vars)
      (everyg #(sumo % sum) cols)
      (everyg #(sumo % sum) rows)
      (sumo anti-diag sum)
      (sumo diag sum))))

(defn magic-square [values]
  (let [;; I wanted to try core.logic, but can't do it with floats
        ;; so I solve the integer version instead
        int-values (->> values
                        (map  #(* 2 %))
                        (map int)
                        vec)
        [[sol _]] (run-solve int-values)
        sqr (->> sol
                 (partition 3)
                 (map vec)
                 (into []))]
    sqr))
;; (magic-square values)
;; => [[3 8 7] [10 6 2] [5 4 9]]

;; (with-out-str (time (magic-square values)))
;; => "\"Elapsed time: 24800.813458 msecs\"\n"
