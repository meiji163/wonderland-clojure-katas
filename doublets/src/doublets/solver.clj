(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words-file-path
  (reduce str
          (System/getProperty "user.dir")
          "/resources/words.edn"))

(def words
  (read-string (slurp words-file-path)))

(defn w-dist [word1 word2]
  (reduce (fn [count pr]
            (if (= (first pr) (second pr))
              count
              (inc count)))
          0
          (map vector word1 word2)))

(defn linked? [word1 word2]
  "if words are the same length and differ by one"
  (and (= (count word1) (count word2))
       (= 1 (w-dist word1 word2))))

(defn linked-words [w]
  (filter (partial linked? w) words))

(def linked-words-memo (memoize linked-words))

(defn dfs-step [dfs-state] "advance depth first search one step"
  (let [{path :path
         visited :visited
         target :target} dfs-state
        [cur & rest-nodes] path

        ;; unvisited neighbors
        nbrs (filter #(not (contains? visited %))
                     (linked-words-memo cur))]
    (cond
      (empty? path) (assoc dfs-state
                           :done :notfound)
      (= target cur) (assoc dfs-state
                            :done :found)
      (empty? nbrs) (assoc dfs-state
                           :path rest-nodes)
      :else
      (let [nbr (first nbrs)
            new-path (cons nbr path)
            new-vis (conj visited nbr)]
        (assoc dfs-state
               :path new-path
               :visited new-vis)))
    ))

(defn run-dfs [word1 word2]
  (let [start-state {:path [word1]
                     :visited #{word1}
                     :target word2}]
   (last
    (take-while
     #(not (contains? % :done))
     (iterate dfs-step start-state )))))

(defn doublets [word1 word2]
  (-> (run-dfs word1 word2)
       :path
       reverse
       vec))
