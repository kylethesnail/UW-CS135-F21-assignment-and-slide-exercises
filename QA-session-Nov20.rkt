;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname QA-session-Nov20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (compute q)
  (lambda (n m)
    (lamnda (n)
      (lambda (k)
        (- k (* n m))))))

;;make-comparator: (listof Num) -> (Num -> Nat)
(define (make-comparator num-list)
  (lambda (threshold)
    (num-bigger num-list threshold)))

;;stepping, always do the outside ones first 
(define something
  (lambda (a b)
    (lambda (x)
      (lambda (w x y)
        (* b x y)))))

(((something 1 2) 4) 4 5 6)

=> ((((lambda (a b)
        (lambda (x)
          (lambda (w x y)
            (* b x y))))1 2) 4) 4 5 6)

=>(((lambda (x)
          (lambda (w x y)
            (* 2 x y))) 4) 4 5 6)

=>((lambda (w x y)
     (* 2 x y)) 4 5 6)

=> (* 2 5 6)

=>

;;(try-tests test-list x) produces the result of evaluating every test in test-list on x

(check-expect (try-tests (list odd? zero?) 4) (list false false))

;;try-tests: (listof (X -> Bool))X

(define (try-tests test-list x)
  (cond [(empty? test-list) empty]
        [else (cons ((first test-list) x)
                    (try-tests (rest test-list) x))]))