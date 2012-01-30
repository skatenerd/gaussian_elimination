(ns gaussian_elimination.matrix_spec
  (use [gaussian_elimination.matrix]
       [speclj.core]))
(describe "matrix stuff"
  (with cur-matrix [[64 8 1]
                    [49 7 1]
                    [36 6 1]])
  (it "swaps correctly"
   (let [swapped (swap @cur-matrix 0 1)
         desired [[49 7 1]
                  [64 8 1]
                  [36 6 1]]]
     (should= desired swapped)))
  (it "pops cols correctly"
    (let [popped (remove-last-col @cur-matrix)
          desired [[64 8]
                   [49 7]
                   [36 6]]]
      (should= desired popped)))

  (it "finds the last col correctly"
    (let [desired [1 1 1]]
      (should= desired (last-col @cur-matrix))))

  (it "replaces correcly"
    (let [replaced (replace-row @cur-matrix 0 (list 1 1 1))
          desired [[1 1 1]
                   [49 7 1]
                   [36 6 1]]]
      (should= desired replaced)))
  (it "gets rows"
      (should= [64 8 1] (get-row @cur-matrix 0)))
  (it "gets cols"
      (should= [64 49 36] (get-col @cur-matrix 0)))
  (it "inserts cols"
    (let [final-col [9 9 9]
          appended (add-col @cur-matrix final-col)]
      (should= [9 9 9] (get-col appended 3))
      (should= [64 49 36] (get-col appended 0))))
)


(run-specs)
