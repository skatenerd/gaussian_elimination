(ns gaussian_elimination.gaussian
  (use [gaussian_elimination.matrix])
  (use [gaussian_elimination.vector])
  (use [clojure.contrib.math]))
(declare pivot triangulate)

(defn gaussian-elimination [m evaluations]
  22)

(defn eliminate [pivot to-trim column]
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


(defn row-at-bottom [matrix pivot-row-idx]
  (>= (inc pivot-row-idx) (count matrix)))


(defn safe-append [first second]
  (if (empty? second)
    first
    (apply conj (vec first) (vec second))))

(defn eliminate-col [matrix pivot-row-idx col-idx]
  (if (row-at-bottom matrix pivot-row-idx)
    matrix
    (let [untouched-rows (take (inc pivot-row-idx) matrix)
          to-mutate (drop (inc pivot-row-idx) matrix)
          pivot (nth matrix pivot-row-idx)
          mutated (for [row to-mutate]
                    (eliminate
                      pivot
                      row
                      col-idx))]
      (safe-append untouched-rows mutated))))

(defn iter-triangulation [triangulated remaining col-idx]
  (let [pivoted-remaining (pivot
                            remaining
                            0
                            col-idx)
        mutated (eliminate-col
                  pivoted-remaining
                  0
                  col-idx)
        new-col-idx (inc col-idx)]
    (triangulate
      (conj triangulated
            (first mutated))
      (drop 1 mutated)
      new-col-idx)))

(defn triangulate
  ([matrix] (triangulate [] matrix 0))
  ([triangulated remaining col-idx]
   (if (empty? remaining)
     triangulated
     (iter-triangulation triangulated remaining col-idx))))

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


(defn back-substitute
  ([m evaluations] (back-substitute m evaluations [] (dec (count (last m)))))
  ([truncated-matrix truncated-evaluations answers-so-far idx-of-unknown]
   (if (neg? idx-of-unknown)
     answers-so-far
     (let [relevant-row (last truncated-matrix)
           nonzero-entries (subvec relevant-row idx-of-unknown)
           summable (subvec nonzero-entries 1)
           first-nonzero (first nonzero-entries)
           evaluation (last truncated-evaluations)
           combination-of-known-vars (apply 
                                       +
                                       (map
                                         *
                                         answers-so-far
                                         summable))
           remaining-qty (-  evaluation combination-of-known-vars)
           new-known (/ remaining-qty first-nonzero)]
       (back-substitute
         (pop truncated-matrix)
         (pop truncated-evaluations)
         (cons new-known answers-so-far)
         (dec idx-of-unknown))))))

