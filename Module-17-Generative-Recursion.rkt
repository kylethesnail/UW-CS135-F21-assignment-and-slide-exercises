;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Module-17-Generative-Recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;-------------------------------Generative Recursion in general-----------------------------------
;;Generative-Recursion: parameters does not keep getting smaller!
;;i.e Collatz list

(define (collatz-list n)
  (cons n (cond
            [(= n 1) empty]
            [(even? n) (collatz-list (/ n 2))]
            [ else   (collatz-list (+ 1 (* 3 n)))])))

(collatz-list 27)

;;--------------Exercise 1. ------------------------
;;------------Taylor Series-------------------------
;;(ln x) calcualtes the log base e of any positive number num

;(define (ln x)
;  (local [(define (ln-small x k)
;            (cond [(= k 0) #i0]
;                  [else (- (ln-small x (- k 1))
;                           (/ (expt (- 1 x) k) k))]))]
;
;    (foldr +  (ln-small x 20) (build-list k (lambda (x k)) (/(expt (- 1 x) k) k)))))

(define (ln x)
  (local [(define (ln-small x k)
            (cond [(= k 0) #i0]
                  [else (- (ln-small x (- k 1))
                           (/ (expt (- 1 x) k) k))]))]
    (cond [(<= x 0) false]
          [(<= x 2) (ln-small x 20)]

          [else (+ (ln-small 2 20)

                   (ln (/ x 2)))])))

  ;;----------------QUICKSORT-------------------------------------------
  ;;Quick-sort: divide and conquer: Divide a problem into smaller subproblems
  ;;                                Recursively solve each one
  ;;                                Combine the solution to solve the original probelm

  ;;i.e:  quicksort tactic on list (list 9 4 15 2 12 20), pick the first element 9 as pivor then the subproblems are (list 4 2) and (list 15 12 20)
  ;;      Then recursivley sort the two sublists we shall have (list 2 4) and (list 12 15 20)
  ;;      Eventually combine them with the pivor which would give the answer
  ;(append (list 2 4)(list 9)(list 12 15 20))
  ;
  ;;;defining a pivot and subproblems
  ;(define pivot (first lon))
  ;
  ;;;define a predicate function
  ;(lambda (x) (< x pivot))
  ;
  ;;;use filter function to create a list of items less than the pivot
  ;(filter (lambda (x) (< x pivot)) lon)
  ;
  ;;;also use filter function to create a list of items bigger than the pivot
  ;(filter (lambda (x) (> x pivot)) lon)

  ;;(my-quicksort lon) sorts lon in non-decreasing order
  ;(check-expect (my-quicksort '(5 3 9)) '(3 5 9))
  ;(check-expect (my-quicksort '(3 5 2 4 9 10 11 1 6)) '(1 2 3 4 5 6 9 10 11))
  (check-expect (my-quicksort '(4 7 -3 4 2 9 1 8 2 1 3))(list -3 1 2 3 4 7 8 9))


  (define (my-quicksort lon)
    (cond [(empty? lon) empty]
          [else (local [(define pivot (first lon))
                        (define less (foldr (lambda (x rror)(cond [( < x pivot)(cons x rror)]
                                                                  [else rror])) empty lon))
                        (define greater (filter (lambda (x)(> x pivot))
                                                (rest lon)))]
                  ;                      (append (my-quicksort less)
                  ;                              (list pivot)
                  ;                              (my-quicksort greater)))]))
                  (append (my-quicksort less)
                          (cons pivot
                                (my-quicksort greater))))]))

  ;(foldr (lambda (x rror)(cond [(x < pivot)(cons x rror)]
  ;                             [else rror])) empty lon)

  ;;-----------------------------Example: breaking string into lines----------------------
  ;;"control symbol "\n"
  ;;Consider converting a string such as "one\ntwo\nthree" into a list of strings (list "one" "two" "three"), one for each line
  ;;now instead of thinking of the list as a list of characters, think of them as list of "lines"
  ;;And a list of lines is either empty or A LINE FOLLOWED (FIRST-LINE)BY MANY MORE LINES (REST-LINES)

  ;;helper-function-1
  ;;similar to first of list of X this function produces first of "list of lines" 
  ;;(first-line loc) produces longest newline-free prefix of loc
  ;;Examples:
  (check-expect (first-line empty) empty)
  (check-expect (first-line '(#\a#\newline)) '(#\a))
  (check-expect (first-line (string->list "abc\ndef")) '(#\a#\b#\c))

  (define (first-line loc)
    (cond [(empty? loc) empty]
          [(char=? (first loc) #\newline) empty] ;;if we encounter "#\newline" we have gone as far as we would like to go then we produce empty to anchor down the list
          [else (cons (first loc)(first-line (rest loc)))]))

  ;;helper-function-2
  ;;(rest-of-lines loc) produces the rest of the loc with everything up to and including the first newline removed
  ;;Examples:
  (check-expect (rest-of-lines empty) empty)
  (check-expect (rest-of-lines '(#\a #\newline)) empty)
  (check-expect (rest-of-lines '(#\a #\newline #\b #\c)) '(#\b #\c))

  (define (rest-of-lines loc)
    (cond [(empty? loc) empty]
          [(char=? (first loc) #\newline)(rest loc)]
          [else (rest-of-lines (rest loc))]))

  ;;"listof lines" template
  ;(define(loc->lol loc)
  ;  (local[(define frstline (first-line loc))
  ;         (define restline (rest-of-lines loc))]
  ;    (cond [(empty? loc) empty]
  ;          [else ... frstline...(loc->lol (rest-of-lines loc))])))


  ;;now convert the template to the general solution to the question
  (define (loc->lol loc)
    (local [(define frstline (first-line loc))
            (define restline (rest-of-lines loc))]
      (cond [(empty? loc) empty]
            [else (cons (list->string frstline)
                        (loc->lol restline))])))

  (define (string->lines s)(loc->lol (string->list s)))
     
  (check-expect (string->lines "one\ntwo\nthree") (list "one" "two" "three"))


  ;;---------------------Exercise 2--------------------------------------
  ;;Mccarthy 91 function
  ;(define (collatz-list n)
  ;  (cons n (cond
  ;            [(= n 1) empty]
  ;            [(even? n) (collatz-list (/ n 2))]
  ;            [ else   (collatz-list (+ 1 (* 3 n)))])))
  ;
  ;(collatz-list 27)

  (define (mc-91 n)
    (cond[(> n 100) (- n 10)]
         [else (mc-91 (mc-91 (+ 11 n)))]))

  ;;-------------------Exercise 3---------------------------------------
  ;;product of prime factors
  ;;(pfd n) produces a list of prime factors for natural number n
  (check-expect (pfd 42)'(2 3 7))
  (check-expect (pfd 24)'(2 2 2 3))

  ;;Nat -> (listof Nat)

  (define (pfd n)
    (local [(define (pfd-from start n)
              (cond [(equal? n start) (cons start empty)]
                    [(zero? (remainder n start))(cons start (pfd-from start (/ n start)))]
                    [else(pfd-from (add1 start) n)]))]
      (pfd-from 2 n)))
  