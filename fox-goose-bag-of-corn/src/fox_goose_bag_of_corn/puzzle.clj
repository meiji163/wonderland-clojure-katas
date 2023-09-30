(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[:fox :goose :corn :you] [:boat] []])

;; A move is take :fox, :goose, :corn across, or :you (cross alone)
(def all-moves [:fox :goose :corn :you])

;; These simulate set operations on vectors
(defn rm-elems [v & es]
  (vec
   (reduce (fn [xs e]
             (remove #(= e %) xs))
           v es)))

(defn add-elems [v & es]
  (vec
   (reduce (fn [xs e]
             (if (.contains xs e)
               xs
               (conj xs e)))
           v es)))

(defn make-move [pos move]
  (let [[left mid right] pos]
    (cond
      (.contains left :you) [(rm-elems left :you move) mid (add-elems right :you move)]
      (.contains right :you) [(add-elems left :you move) mid (rm-elems right :you move)]
      :else pos)))

(defn valid? [pos]
  (letfn [(side-invalid? [s]
            (cond (.contains s :you) false
                  :else
                  (or
                   (and (.contains s :fox) (.contains s :goose))
                   (and (.contains s :goose) (.contains s :corn)))))]
    (let [[left _ right] pos]
      (and (not (side-invalid? left))
           (not (side-invalid? right))))
    ))

(defn encode [pos mv]
  (let [left (first pos)]
    (cons mv
     (map (fn [x] (.contains left x)) all-moves))))

(defn run-search [ss]
  (let [{pos :pos
         mvs :moves
         vis :visited} ss
        [left _ right] pos
        last-mv (first mvs)

        cur-side (if (.contains left :you) left right)
        next-poss (map (partial make-move pos) cur-side)

        do-move? (fn [pr]
                   (let [[mv pos] pr]
                     (and (not= last-mv mv)
                          (valid? pos)
                          (not (contains? vis (encode pos mv))))))

        next-mv-poss (filter do-move?
                             (map vector cur-side next-poss))]
    (cond
      (= (set right) (set all-moves)) (assoc ss :done true)
      (empty? next-mv-poss) (assoc ss  ; undo last move
                                   :pos (make-move pos last-mv)
                                   :moves (rest mvs))
      :else
      (let [[next-mv next-pos] (first next-mv-poss)
            next-vis (conj vis (encode next-pos next-mv))]
        (assoc ss
               :visited next-vis
               :moves (cons next-mv mvs)
               :pos next-pos))
      )))

(defn solve [ss]
  (last (take-while
         #(not (:done %))
         (iterate run-search ss))))

(defn crossing-pos [pos mv]
  (let [[left mid right] pos
        crossing (if (= mv :you)
                   (conj mid :you)
                   (conj mid :you mv))]
    [(rm-elems left :you mv ) crossing (rm-elems right :you mv)]))

(defn river-crossing-plan []
  (let [start-state {:pos start-pos
                     :moves []
                     :visited #{}}

        soln (solve start-state)
        mvs (reverse (:moves soln))
        pos-seq (reduce
                 (fn [poss mv]
                   (conj poss (make-move (last poss) mv)))
                 [start-pos] mvs)
        last-pos (last pos-seq)
        ;; add in crossing steps
        cross-seq (map #(apply crossing-pos %)
                       (map vector pos-seq mvs))

        full-seq (vec (interleave pos-seq cross-seq))]
    (conj full-seq last-pos)))
