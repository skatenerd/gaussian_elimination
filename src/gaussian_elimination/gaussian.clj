(ns gaussian_elimination.gaussian
  (use [gaussian_elimination.matrix])
  (use [gaussian_elimination.vector])
  (use [clojure.contrib.math]))
(declare pivot triangulate back-substitute)

(defn gaussian-elimination [m evaluations]
  (let [augmented (add-col m evaluations)
        triangulated (triangulate augmented)
        evaluations (last-col triangulated)
        triangulated-without-evaluations (remove-last-col triangulated)]
        (back-substitute
          triangulated-without-evaluations
          evaluations)
        ))

(defn build-row [degree x-val]
  (vec(map #(Math/pow x-val %) (reverse (range (inc degree))))))

(defn build-matrix [degree x-vals]
  (vec (for [x x-vals]
    (build-row degree x))))

(defn interpolate [points]
  (let [degree (dec (count points))
        x-vals (map #(first %) points)
        evaluations (map #(second %) points)
        matrix (build-matrix degree x-vals)
        ]
    (gaussian-elimination matrix evaluations)))
        

(defn eliminate-cell [pivot to-trim column]
  (let [pivot-elt (nth pivot column)
        to-trim-elt (nth to-trim column)
        scaling-factor (/ to-trim-elt pivot-elt)
        scaled (scale-vector pivot scaling-factor)
        with-column-eliminated (subtract-vectors to-trim scaled)
        with-zero-enforced (assoc 
                             (vec with-column-eliminated)
                             column
                             0)]
    with-zero-enforced))


(defn row-at-bottom? [matrix pivot-row-idx]
  (>= (inc pivot-row-idx) (count matrix)))


(defn safe-append [first second]
  (if (empty? second)
    first
    (apply conj (vec first) (vec second))))

(defn eliminate-col [matrix pivot-row-idx col-idx]
  (if (row-at-bottom? matrix pivot-row-idx)
    matrix
    (let [untouched-rows (take (inc pivot-row-idx) matrix)
          to-mutate (drop (inc pivot-row-idx) matrix)
          pivot (nth matrix pivot-row-idx)
          mutated (for [row to-mutate]
                    (eliminate-cell
                      pivot
                      row
                      col-idx))]
      (safe-append untouched-rows mutated))))

(defn iter-triangulation [triangulated-rows remaining-rows col-elimination-idx]
  (let [pivoted-remaining-rows (pivot
                            remaining-rows
                            0
                            col-elimination-idx)
        with-eliminated-col (eliminate-col
                              pivoted-remaining-rows
                              0
                              col-elimination-idx)
        new-col-elimination-idx (inc col-elimination-idx)]
    (triangulate
      (conj triangulated-rows
            (first with-eliminated-col))
      (drop 1 with-eliminated-col)
      new-col-elimination-idx)))

(defn triangulate
  ([matrix] (triangulate [] matrix 0))
  ([triangulated-rows remaining-rows col-elimination-idx]
   (if (empty? remaining-rows)
     triangulated-rows
     (iter-triangulation triangulated-rows remaining-rows col-elimination-idx))))

(defn idx-of-max-val [v]
  (apply
    max-key
    #(abs (nth v %))
    (range (count v))))

(defn pivot [matrix pivot-row-idx pivot-col-idx]
  (if (empty? matrix)
    matrix
    (let [relevant-col (get-col matrix pivot-col-idx)
          row-with-max-abs-val (idx-of-max-val relevant-col)]
      (swap matrix pivot-row-idx row-with-max-abs-val))))

(defn get-linear-combination [coefficients basis]
  (apply
    +
    (map
      *
      coefficients
      basis)))

(defn iterate-back-substitution 
  [truncated-matrix 
   truncated-evaluations
   answers-so-far
   idx-of-unknown]
   (let [relevant-row (last truncated-matrix)
         nonzero-entries (subvec relevant-row idx-of-unknown)
         entries-with-known-values (subvec nonzero-entries 1)
         first-nonzero (first nonzero-entries)
         evaluation (last truncated-evaluations)
         combination-of-known-vars (get-linear-combination
                                     entries-with-known-values
                                     answers-so-far)
         remaining-qty (-  evaluation combination-of-known-vars)
         new-known (/ remaining-qty first-nonzero)]
     (back-substitute
       (pop (vec truncated-matrix))
       (pop (vec truncated-evaluations))
       (cons new-known answers-so-far)
       (dec idx-of-unknown))))

(defn back-substitute
  ([m evaluations] (back-substitute m evaluations [] (dec (count (last m)))))
  ([truncated-matrix truncated-evaluations answers-so-far idx-of-unknown]
   (if (neg? idx-of-unknown)
     answers-so-far
     (iterate-back-substitution
       truncated-matrix
       truncated-evaluations
       answers-so-far
       idx-of-unknown))))

