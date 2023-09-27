(ns wonderland-number.finder)

(defn digits [^long n]
  (re-seq #"\d" (str n)))

(defn permutation? [l1 l2]
  (= (sort l1) (sort l2)))

(defn wonderland? [^long n]
  (let [nums [(* 2 n) (* 3 n) (* 4 n) (* 5 n) (* 6 n)]
        n-digs (digits n)]
    (every? true?
     (map #(permutation? (digits %) n-digs)
      nums))
    ))

(defn wonderland-number []
  (loop [n 100000]
    (when (< n 166667)
      (if (wonderland? n)
        n
        (recur (inc n))))
    ))

(wonderland-number)
;; => 142857
