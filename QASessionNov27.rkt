;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname QASessionNov27) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;---------------about foldl and foldr-------------------------------------
;;listof-x-template: (lisstof x)-> Any
;(define (listof-x-template lox)
;  (cond [(empty? lox) (something-with-base-case)]
;        [else (combine (first lox)(listof-x-template (rest lox)))]))
;
;;;foldr replicates this process
;(foldr
; (combine (first lox)(listof-x-template (rest lox)))
; (something-with-base-case) lox)

;;(pair-sums-to-val val num-list) produces true if a pair of elements in num-list sums to val, false otherwise
;;Example:
(check-expect (pair-sums-to-val 3 '(8 7 11 13 5 2 1)) true)
(check-expect (pair-sums-to-val 4 '(11 8 -3 18 9)) false)

(define (pair-sums-to-val val num-list)
  (foldr
   ;; lambda: Num Bool -> Bool
   (lambda (item rror)
     (cond [rror true]
           [(foldr
             (lambda (x rror)(cond[rror true]
                              [(= x item) rror]
                              [(equal? val (+ x item)) true]
                              [else false])) false num-list) true]
           [else false]))
   false
   num-list))
           
;;pair-sums-to-val: Num (listof Num) -> Bool
;;Requires: every element in num-list is unique, and (length num-list >= 2)

;(define (pair-sums-to-val val num-list)
;  (foldr (lambda (x rror)(cond []

;;(item-sums-to-val val item num-list) produces true if some element of num-list + item sums to val
(check-expect (item-sums-to-val 3 1 '(8 7 11 2 13 5 1)) true)

;; item-sums-to-val: Num Num (listof Num) -> Bool
(define (item-sums-to-val val item num-list)
  (foldr (lambda (x rror)(cond[rror true]
                              [(= x item) rror]
                              [(equal? val (+ x item)) true]
                              [else false])) false num-list))

;;---------------------------Generative Recursion--------------------------------
;;make the parameters what you want but you need to ensure that something gets smaller at every recursion

;;a more complex example of foldr
;;(keep-even-pos arblist) produces the element at arblist at even positions

(check-expect (keep-even-pos (list 1 2 3 4))(list 1 3))
(check-expect (keep-even-pos (list "cow" "pig" "crow" "shark" "rooster")) (list "cow" "crow" "rooster"))

;;keep-even-pos: (listof X) -> (listof X)
(define (keep-even-pos arblist)
  ;; lambda: X (list Nat (listof X)) -> (list Nat (listof X))
  (second (foldl (lambda (frst acc)
           (local [(define pos (first acc))
                   (define result-so-far (second acc))]
             (cond [(even? pos)(list (add1 pos)(append result-so-far (list frst)))]
                   [(odd? pos)(list (add1 pos) result-so-far)]))) (list 0 empty) arblist)))
 
;; quick reverse with foldl
(foldl cons empty '(1 2 3 4))
(foldl (lambda (frst rror) (cons frst rror)) empty '(1 2 3 4))
(foldr (lambda (item1 item2 rror) (cons (+ item1 item2) rror)) empty '(1 2 3 4 5) '(8 2 2 1 3))