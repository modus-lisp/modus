(print (flet ((f (x) (* x x))) (f 5)))
(print (labels ((g (n) (if (< n 2) n (+ (g (- n 1)) (g (- n 2)))))) (g 10)))
(print (labels ((h (x) (+ x 1))) (h 3)))
