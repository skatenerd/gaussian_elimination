(ns gaussian.elimination.matrix
  (use [gaussian.elimination.vector]))

(defn swap [m top bottom]
  (let [casted (vec m)]
  (assoc casted bottom (casted top) top (casted bottom))))

(defn replace-row [m idx new-row]
  (assoc m idx (vec new-row)))

(defn get-row [m idx]
  (m idx))

(defn get-col [m idx]
  (for [row m]
    (row idx)))

(defn add-col [m entries]
  (for [i (range (count m))]
    (conj (nth m i) (nth entries i))))

(defn remove-last-col [m]
  (for [row m]
    (pop row)))

(defn width [m]
  (count (first m)))

(defn last-col [m]
  (get-col m (dec (width m))))
