(ns tiny-maze.solver)

;;; Maze is a mxn grid with the start at [0 0] and end at [m n]
(def max-solve-steps 1000)
(def max-dist 10000)

(defn in-maze? [dims xy]
  (let [[xdim ydim] dims
        [x y] xy]
    (and (<= 0 x) (<= 0 y)
         (> xdim x) (> ydim y))))

(defn maze-dims [maze]
  [(count maze) (count (first maze))])

(defn neighbors [maze xy]
  (let [dims (maze-dims maze)
        [x y] xy]
    (filter (partial in-maze? dims)
            [[(inc x) y] [x (inc y)]
             [(dec x) y] [x (dec y)]])))

(defn coord-free? [maze xy]
  (not= 1 (get-in maze xy)))

(defn solve-step [maze search-state]
  "advance the Djikstra maze search one step"
  (let [[xdim ydim] (maze-dims maze)
        {boundary :boundary
         visited :visited
         last :last
         dist :dist} search-state

        dist-to-start (fn [p]
                        (or (get dist p) max-dist))

        point (first (sort-by dist-to-start boundary))
        nbrs (filter #(and (coord-free? maze %)
                           (not (contains? visited %)))
                     (neighbors maze point))

        ;; new state
        new-boundary (concat nbrs
                             (remove #(= % point) boundary))
        new-visited  (conj visited point)]
    (if
        (= :E (get-in maze point)) ;; reached the end
        (conj search-state [:done true])

        ;; update neighbor points
        (letfn [(update-nbrs [state nbr]
                  (let [dist (:dist state)
                        last (:last state)
                        point-dist (get dist point)
                        nbr-dist (get dist nbr max-dist)]
                    (if (< (inc point-dist) nbr-dist)
                      (assoc state
                             :dist (conj dist [nbr (inc point-dist)])
                             :last (conj last [nbr point]))
                      state)))]
          (reduce update-nbrs
                  (assoc search-state
                         :boundary new-boundary
                         :visited new-visited)
                  nbrs)))))

(defn get-path [xy lastmap]
  (letfn [(path-step [path]
            (conj path
                  (get lastmap (last path))))

          (not-done? [path]
            (and (< (count path) max-solve-steps)
                 (not= nil (last path))))]
    (last
     (take-while not-done? (iterate path-step [xy])))))

(defn solve-maze [maze]
  (let [start-state {:visited #{}
                     :boundary [[0 0]]
                     :last {}
                     :dist {[0 0] 0}}
        end-state (last
                   (take-while #(not (:done %))
                               (iterate (partial solve-step maze) start-state)))
        [xdim ydim] (maze-dims maze)
        end-coord [(dec xdim) (dec ydim)]
        lastmap (:last end-state)
        path (reverse
              (get-path end-coord lastmap))]

    ;; fill in the path
    (vec (for [x (range xdim)]
           (vec (for [y (range ydim)]
                  (if (.contains path [x y])
                    :x
                    (get-in maze [x y]))))
           ))
    ))

(comment (let [maze [[:S 0 0 1 1 1 1 0]
                     [0 0 0 0 0 0 0 0]
                     [0 0 1 0 0 0 0 0]
                     [0 0 0 0 1 0 0 1]
                     [0 0 0 0 1 1 0 0]
                     [0 1 0 0 0 0 0 0]
                     [0 1 0 0 0 0 1 1]
                     [0 0 0 0 1 0 0 :E]]]
           (solve-maze maze)))
;; [[x 0 0 1 1 1 1 0]
;;  [x 0 0 0 0 0 0 0]
;;  [x 0 1 0 0 0 0 0]
;;  [x x 0 0 1 0 0 1]
;;  [0 x x 0 1 1 0 0]
;;  [0 1 x x 0 0 0 0]
;;  [0 1 0 x x x 1 1]
;;  [0 0 0 0 1 x x x]]
