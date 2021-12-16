;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Module-16-Test-Ground) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;-------------------self-check m16-100-----------------
((lambda (x) (+ 3 x)) 4)

;;-------------------self-check m16_105-----------------
((lambda (x y) (- (min x y) (max x y))) 3 2)

;;Introducing Lambda: From pain-staking local helper function to anonymous functions that are just not useful elsewhere

(define (eat-apples lst)
  (local [(define (not-an-apple? item)
            (not (symbol=? item 'apple)))]
    (filter not-an-apple? lst)))

(eat-apples (list 'apple 'apple 'apple 'pear 'pineapple 'lima 'india))



(define (make-between n m)
  (local [(define (f x)(and (>= x n)(<= x m)))] f))

(filter (make-between 2 4) (list 1 3 5 2 2 3 4 9 8))

;;Now introducing lambda
(define (eat-apples-v2 lst)
  (filter (lambda (item) (not (symbol=? item 'apple))) lst))

(define lst '(3 5 9 5 5 4))
(filter (lambda (x) (= 5 x)) lst)                  
(filter (lambda (x) (and (<= 3 x) (<= x 5))) lst)   
(filter (lambda (s) (char>? s #\Z)) '(#\B #\a #\y))

;;Tracing through (eat-apple-v2 lst)
(define (eat-apples-v3 lst)
  (filter (lambda (item) (not (symbol=? item 'apple))) lst))

;;;now define an input application
;(eat-apples-v2 '(pear apple))
;=> (filter (lambda (item)(not (symbol=? item 'apple))) '(pear apple))
;=> (cond [(empty? '(pear apple))empty]
;         [((lambda (item)(not (symbol=? item 'apple)))
;           (first '(pear apple)))]
;         (cons (first 'pear apple))

;;self-check m16_110-----------------------------------
([(lambda (x)(lambda (y)(- (min x y) (max x y))))3] 2)

;;self-check m16_115-----------------------------------(17/69)
;((lambda (x)
 ;  (lambda (y)
  ;   (- (min x y) (max x y)))) 
  ;3 2)

;;self-checl m16_120--------------------------------(17/69)-------------
;;PAY ATTENTION TO WHAT EACH IS DOING FOR A BETTER UNDERSTANDING OF LAMBDA-------------------------------------
;;A
(define (recip x) (lambda (x) (/ 1 x))) ;;this is returning a function which calculates reciprocal by binding another function to it, in (recip x) the x parameter does not matter
((recip 999) 10) ;; HERE 999 is abandoned

;;;B
;(define (recip-2)(lambda (x) (/ 1 x))) ;; this is also returning a function which calculates the reciprocal which is binded to a function which takes in no parameters,will yield an error at our current language level
;((recip) 10)

;;C

(define recip-3 (lambda (x)(/ 1 x))) ;; defines constant "recip-3" which is a function that calculates the reciprical of the provided number
(recip-3 10)

;;D 
;;(lambda (recip x) (/ 1 x)) ;; by itself is a lambda expression, takes two parameters, one being recip, the other is x, recip will be ignored
;;to use it we have to add extra set of brackets
((lambda (recip x) (/ 1 x)) 999 10)

;;------------------------------Exercise 1------------------------------------------
;;Using lambda and filter but no named helper functions and write a function that consumes a (listof Str) and produces a list contaning all strings with a length of 4
(check-expect (keep4 '("There's" "no" "fate" "but" "what" "we" "make" "for""ourselves")) '("fate" "what" "make"))

;;keep4: (X -> Bool)(listof X) -> (listof X)

(define (keep4 los)
  (filter (lambda (item) (equal? 4 (string-length item))) los))

;;------------------------------Character-Transformation Example----------------------------------
;;Function transform consumes a string and transforms it according to a set of rules
;(check-expect (transform "abracadabra"...) "bbrbcbdbbrb")
;(check-expect (transform "Testing 1-2-3"...) "TESTING *-*-*")

;;suppose we supplied (transform...) with a list of question/answer pairs:

;;A TransformSpec is one of:
;; *empty
;; *(cons (list Question Answer) TransformSpec) a list of question answer pairs, just like cond

;;A Question is a (Char -> Bool)
;;An Answer is a (Char -> Char) consumes a character and returns a (possibly different) character

(check-expect (transform "Testing 1-2-3" (list (list char-lower-case? char-upcase)
                                               (list char-numeric? (lambda (ch) #\*)))) "TESTING *-*-*")

;;TransformSpec is one of:
;;* empty
;;* (cons (list Question Answer) TransformSpec)

;;transform: Str TransformSpec -> Str
(define (transform s spec)
  (list->string (trans-loc (string->list s) spec)))

;;helper-function
;;trans-loc (listof Char) TransformSpec  -> (listof Char)
(check-expect (trans-loc (list #\a #\9)
                        (list (list char-lower-case? char-upcase)))
                           (list #\A #\9))

(define (trans-loc loc spec)
  (cond [(empty? loc) empty]
        [(cons? loc) (cons (trans-char (first loc) spec)
                           (trans-loc (rest loc) spec))]))

(define (trans-char ch spec)
  (cond [(empty? spec) ch] ;; return the character unchanged if we did not find a case which applies in spec
        [((first (first spec)) ch)((second (first spec)) ch)];; go through the specs row by row until we find one that applies to ch, then apply the function in the second part on the ch to transform it as desired
        [else (trans-char ch (rest spec))]))

(check-expect (transform "abracadabra" (list (list (lambda (ch) (char=? ch #\a)) ;;Question
                                                   (lambda (ch) #\b))));;Answer
                                                         "bbrbcbdbbrb")

;; we may write some utility functions:
(define (is-char? c1) (lambda (c2) (char=? c1 c2)))
(define (always c1) (lambda (c2) c1))

;;-------------------self-check: m16_220-------------------
(transform "I love Racket"
  (list (list (lambda (ch) true)
              (lambda (ch) (next-char ch)))
        (list char-whitespace? 
              (lambda (ch) #\.))))

;;utility functions:
(define is-char? (lambda (ch)(character?
                           



    