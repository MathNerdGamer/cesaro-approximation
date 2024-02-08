; cesaro.scm
; Implements Cesaro's approximation for pi using Monte Carlo experiments.

; Displays a line, followed by newline.
(define (display-line t)
  (display t)
  (newline)
  t)

; Implements the MINSTD LCG.
(define minstd
  (lambda(X)
    (lambda()
      (define (gen n) (modulo (* n 48271) 2147483647))
      (set! X (gen X)) X)))

; Creates an instance of MINSTD with seed 1.
(define rand (minstd 1))

; Estimate pi by doing n Cesaro experiments.
(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

; Return the result of testing is two random numbers are relatively prime. 
(define (cesaro)
  (= (gcd (rand) (rand)) 1))

; Run experiments using the provided procedure and return the tallied results.
(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (-1+ remaining)
                 (1+ passed)))
          (else
            (iter (-1+ remaining)
                       passed))))
    (iter trials 0))

; Estimates pi using 50000 iterations of Cesaro's method.
(display-line (estimate-pi 50000))
