(ns tiny-maze.solver)

;;; Maze is a mxn grid with the start at [0 0] and end at [m n]
(def max-solve-steps 1000)

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
             [(dec x) y] [x (dec y)]])
    ))

(defn coord-free? [maze xy]
  (not= 1 (get-in maze xy)))

(defn solve-step [maze search-state]
  "advance the maze search one step"
  (let [[xdim ydim] (maze-dims maze)
        dist-to-end (fn [p] (+ (Math/abs (- xdim (first p)))
                               (Math/abs (- ydim (second p)))))
        {bdry :boundary
         vistd :visited
         last :last} search-state

        point (first (sort-by dist-to-end bdry))
        nbrs (filter #(and (coord-free? maze %)
                           (not (contains? vistd %)))
                     (neighbors maze point))]
    (cond
      ;; reached the end
      (= :E (get-in maze point)) (conj search-state [:done true])

      ;; no reachable coords
      (empty? nbrs) {:visited (conj vistd point)
                     :boundary (remove #(= % point) bdry)
                     :last last}

      :else ;; choose next coord
      (let [next (first (sort-by dist-to-end nbrs))]
        {:visited (conj vistd point)
         :boundary (concat nbrs
                           (remove #(= % point) bdry))
         :last (conj last [next point])}))
    ))

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
                     :last {}}
        end-state (last
                   (take-while #(not (:done %))
                               (iterate (partial solve-step maze) start-state)))
        [xdim ydim] (maze-dims maze)
        end-coord [(dec xdim) (dec ydim)]
        lastmap (:last end-state)
        path (reverse
              (get-path end-coord lastmap))]

    (vec (for [x (range xdim)]
           (vec (for [y (range ydim)]
                  (if (.contains path [x y])
                    :x
                    (get-in maze [x y]))))
           ))
    ))
