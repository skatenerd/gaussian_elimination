(ns gaussian_elimination.gaussian_spec
  (:use [gaussian_elimination.gaussian]
        [gaussian_elimination.matrix]
        [gaussian_elimination.vector]
        [speclj.core]))


(describe "user-friendly wrapper"
  (with coefficients [1 -8 -14 -2 11])
  (with x-vals [5 10 20 30 32])
  (with evaluations [-724, 591, 90371, 581351, 772043])
  (with points (map #(vector %1 %2) @x-vals @evaluations))

  (it "still works with the wrapper"
    (should= @coefficients (interpolate @points))))


(describe "elimination for polynomials: acceptance test?"
  (with coefficients [2 1 4])
  (with degree-two-polynomial [[64 8 1]
                    [49 7 1]
                    [36 6 1]])
  (with evaluations [140 109 82])

  (it "finds the right coefficients"
    (should= @coefficients (gaussian-elimination @degree-two-polynomial @evaluations))
      ))


(describe "triangulation"
  (with coefficients [2 3 -1])
  (with matrix [[2 1 -1]
                [-3 -1 2]
                [-2 1 2]])
  (with evaluations [8 -11 -3])
  (with augmented (add-col @matrix @evaluations))
  (it "grooms for partial pivoting"
    (should= [[-3 -1 2]
              [2 1 -1]
              [-2 1 2]]
             (pivot @matrix 0 0)))
  (it "kills one cell correctly"
    (let [pivot (first @matrix)
          other (second @matrix)
          col-idx 0]
      (should= [0 0.5 0.5] (eliminate pivot other col-idx))))
  
  (it "kills one column correctly"
    (let [col-idx 0
          pivot-row-idx 0
          after-one-iter [[2 1 -1]
                          [0 0.5 0.5]
                          [0 2 1]]
          after-two-iters [[2 1 -1]
                           [0 0.5 0.5]
                           [0 0 -1]]]
                           

      (should= after-one-iter (eliminate-col @matrix col-idx pivot-row-idx))
      (should= after-two-iters (eliminate-col after-one-iter (inc col-idx) (inc pivot-row-idx)))))


  (it "triangulates with pivoting correctly"
    (should= [[-3 -1 2 -11]
              [0.0 (/ 5 3) (/ 2 3) (/ 13 3)]
              [0 0 (/ 1 5) (/ -1 5)]]
             (triangulate @augmented)))
)

(describe "back substition"
  (it "works"
    (let [answers [1 2 3]
          evaluations [9 5 3]
          m [[2 2 1]
             [0 1 1]
             [0 0 1]]]
      (should= answers (back-substitute m evaluations)))))

(run-specs)

