(ns gaussian_elimination.vector
  "vector manipulation library.
  some ideas taken from mark reid's minilight in clojure"
  (:require [clojure.contrib.generic.math-functions :as math])
  )

(defn dot-product [frst sec] 
  (reduce + (map * frst sec)))

(defn scale-vector [v f]
  (let [times-f (fn [x]
                  (* x f))]
    (map times-f v)))

(defn add-vectors [first second]
  (map + first second))

(defn subtract-vectors [first second]
  (map - first second))

(defn vector-norm [v]
  (Math/sqrt (dot-product v v)))

(defn normalize [v]
  (scale-vector v (/ 1 (vector-norm v))))

(defn get-normal-vector [v1 v2]
  (map - v1 v2))
