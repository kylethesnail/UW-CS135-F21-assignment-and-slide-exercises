;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Module15-Function-As-Values) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (f g h x y)
    (h (g x y) (g x x)))
(f * - 3 5)

;;-----------------------Example 1 and 2: Eat-apples and keep-odds---------------------
;;Eat-Apples: 
(define (eat-apple lst)
  (cond [(empty? lst)empty]
        [(not (symbol=? 'apple (first lst)))
              (cons (first lst)(eat-apple (rest lst)))]
        [else (eat-apple (rest lst))]))

;;Keep-odds:
(define (keep-odds lst)
  (cond[(empty? lst) empty]
       [(odd? (first lst))(cons (first lst)(keep-odds (rest lst)))]
       [else (keep-odds (rest lst))]))

;;Difference here: what we check for the first thing on the list: a symbol 'apple VS an odd number
;;>Abstracting "keep-odds" to "my-filter"
(define (my-filter pred? lst)
  (cond [(empty? lst) empty]
        [(pred? (first lst))(cons (first lst)(my-filter pred? (rest lst)))]
        [else (my-filter pred? (rest lst))]))

;;Notice: We actually have a built in "filter function"
(filter odd? (list 1 2 3 4 5 6 7))

;;not-apple? DEFINE OUR OWN PREDICATE
(define (not-apple? x)(not (symbol=? x 'apple)))

(filter not-apple? (list 'pear 'jujubees 'apple 'apple 'apple))

;;Passing in funcion as arguments 
;;----------------------Exercise 1-------------------------
;;(keep-multiples3 lst) produces a list where only multiples of 3 are kept. use the "filter" function built-in
(define (not-multiples3? x)(not (integer? (/ x 3)))) ;; define our predicate function in this case "not-multiples3?"

(define (keep-multiples3 lst)
  (filter not-multiples3? lst))

;;---------------------Exercise 2----------------------------
;;(keep-multiples2&3 lst) produces a list where only multiples of 2 and 3 are kept
(check-expect (keep-multiples23 (list 1 2 3 4 5 6 7 8 9 10))(list 2 3 4 6 8 9 10))

;;lst -> lst
(define (multiples2&3? x)(or(integer? (/ x 2))
                            (integer? (/ x 3)))) ;; define predicate

(define (keep-multiples23 lst)
  (filter multiples2&3? lst))

;;--------------------Exercise 3------------------------------
;;(keep-in-range lst) produces a list with only numbers between a value range is kept
(check-expect (keep-inrange (list -5 10.1 12 7 30 3 19 6.5 42))(list 10.1 12 30 19))

(define (in-range? x)
  (and (>= x 10)
       (<= x 30)))

(define (keep-inrange lst)
  (filter in-range? lst))

;;-------------------Exercise 4--------------------------------
;;(keep-short lst) keeps all the values in the lst of length at most 6


(define (short-string? str)
  (<= (string-length str) 6))

(define (keep-short lst)
  (filter short-string? lst))

;;------------------Exercise 5--------------------------------
;;(sum-odds-or-evens lst) produces the sum of all evens if there are more evens in the list than odds or the sum of all odds if there are more odds than even
;; (listof Num)-> Num

(define (sum-odds-or-evens lst)
  (local [(define (even-sum lst)
            (cond [(empty? (filter even? lst)) 0]
                  [else (+ (first (filter even? lst))(even-sum(rest (filter even? lst))))]))

          (define (odd-sum lst)
            (cond [(empty? (filter odd? lst))0]
                  [else (+ (first (filter odd? lst))(odd-sum(rest (filter odd? lst))))]))]
    (cond [(> (length (filter odd? lst))(length (filter even? lst))) (odd-sum lst)]
          [else (even-sum lst)])))

;;---------------Producing functions (slide 13/42)---------------------------------------
(define (make-adder n)
  (local [(define (f m) (+ n m))]
    f)) ;; locals can produce strings, numbers, booleans and functions as well...
;;here f is what is being produced by the expresion

; ((make-adder 3)4) ;; double parenthesis here because (make-adder 3) is a sub-expresission which calculates the function to be used in the outter expression
; ;; parameter value n (in this case 3) is "remembered" for as many times as function f would be evaluated
;
;; (make-adder 3)
;;=> (local [(define (f m)(+ 3 m))] f)
;;=> (define (f_42 m) (+ 3 m)) f_42
;;here (make-adder 3) is just the renamed function f_42, which adds 3 to its argument 
(define add2 (make-adder 2))
(define add5 (make-adder 5))

;;-------------Combining Producing and Consuming functions--------------------------------
(define db (list 2 15 10 3 5 8 1 12 9 6))

;;search the data base db between ranges
;; define a function that produces true when the number consumed is between two numbers
;; (in-between consumes two numbers n and m and produces another function f which produces true if the number x is between the range n and m inclusively
(define (in-between n m)
  (local [(define (f x) (and (>= x n) (<= x m)))] f))

;; now apply (in-between n m) nicely with filter function
(filter (in-between 10 30) db)
(filter (in-between 0 10) db)

;;-------------Ex.6 Make-Divisible---------------------------------------------------------
;;(make-divisible? n) produces a predicate function which consumes an Int and returns true if its argument is divisible by n, and false if otherwise

(check-expect (filter (make-divisible? 2)(list 0 1 2 3 4 5 6 7 8 9))(list 0 2 4 6 8))

(define (make-divisible? n)
  (local [(define (divisible-by? m)(integer? (/ m n)))] divisible-by?))

;;;---------------------------Storing functions in lists & Structures: Evaluating expressions with functions---------------------------------------------
;
(define-struct opnode (op args))
;;;;eval: AExp -> Num
;;(define (eval exp)
;;  (cond [(number? exp) exp]
;;        [(opnode? exp)(my-apply-2(opnode-op exp)(opnode-args exp))]))
;;
;;;;Old-version
;;(define (my-apply-1 op args)
;;  (cond [(empty? args) (cond [(symbol=? op '+) 0]
;;                             [(symbol=? op '*) 1])]
;;        [(symbol=? op '+)(+ (eval (first args))
;;                            (my-apply-1 op (rest args)))]
;;        [(symbol=? op '*)(* (eval (first args))
;;                            (my-apply-1 op (rest args)))]))
;
;
;;;New-version
;(define (my-apply op args)
;  (cond [(empty? args) (op )]
;        [else (op (eval (first args))
;                  (my-apply op (rest args)))]))

;;Note this works for any binary function that is also defined for zero arguments
;;Next steps:
;;We know that a structure with n fields can be replaced with an n-element list
;;Quoting gives a really compact notation that may be easier to read


;;i.e
(define trans-table (list (list '+ +)
                          (list '* *)))

;;(lookup-al key al) find the value in al corresponding to key
;;lookup-al: ((listof (listof Sym (Num Num -> Num)) -> (Num Num -> Num))

(define (lookup-al key al)
  (cond [(empty? al) false]
        [(symbol=? key (first (first al))) (second (first al))]
        [else (lookup-al key (rest al))]))

;; i.e: (lookup-al '+ trans-table) produces the function + (identical to Association list back in module 8)

((lookup-al '+ trans-table) 3 4 5)


;;(eval-v2 ex) evaluates the aruthmetic expression ex (26/42)
;; eval: AExp -> Num
(define (eval ex)
  (cond [(number? ex) ex]
        [(cons? ex)(my-apply (lookup-al (first ex) trans-table)
                             (rest ex))])) 

;;(my-apply op exlist) applies op to the list of arguments
;;my-apply: ?? (listog AExp) -> Num

(define (my-apply op args)
  (cond [(empty? args) (op )]
        [else (op (eval (first args))
                  (my-apply op (rest args)))]))

(eval '(+ 1 (* 3 3 3)));; but "+" "*" becomes symbol again! Also note that '(+ 1 (* 3 3 3)) have been made into data and is much more flexible, can be put into file
;;VS
;;(eval (make-opnode + (list 1 (make-opnode * (list 3 3 3)))))




;;--------------------------------Contracts and types (29/42)-----------------------------------------------

;;The contract must capture the fact that the type of the predicate must match the type of the list that gets passed in

;; (make-between n m) produces a function that consumes one number
;;   and produces true if that number is between n and m; false otherwise.
;; Examples:
(check-expect ((make-between 3 5) 2) false)
(check-expect ((make-between 3 5) 3) true)
(check-expect ((make-between 3 5) 6) false)

;; make-between:  Num Num -> ( Num -> Bool)
(define (make-between n m)
  (local [(define (f x) (and (<= n x) (<= x m)))] f))



;; (in-discontinuous-range x lof) returns true if x is within the range
;;    of any of the functions in lof.
;; Examples:
(define d-range
  (list (make-between 0 5) (make-between 10 15) (make-between 20 25)))
(check-expect (in-discontinuous-range 4 d-range) true)
(check-expect (in-discontinuous-range 8 d-range) false)

;; in-discontinuous-range: Num (listof (Num -> Bool)-> Bool
(define (in-discontinuous-range x lof)
  (cond [(empty? lof) false]
        [(cons? lof) (or ((first lof) x)
                         (in-discontinuous-range x (rest lof)))]))



;; (make-in-discontinuous-range lof)
;; Examples: 
(check-expect ((make-in-discontinuous-range d-range) 4) true)
(check-expect ((make-in-discontinuous-range d-range) 8) false)

;; make-in-discontinuous-range: (listof (Num -> Bool)-> (Num -> Bool)
(define (make-in-discontinuous-range lof)
  (local [(define (f x) (in-discontinuous-range x lof))] f))



;;-----------------------------------------EX8.insertion-sort------------------------------


;; (isort pred? lst) sorts the elements of a lst so that adjacent elements satisfy pred?.
;; Examples:
(check-expect (isort < '(3 4 2 5 1)) (list 1 2 3 4 5))
(check-expect (isort > (list 3 4 2 5 1))(list 5 4 3 2 1))
(check-expect (isort string<? (list "can" "ban" "fan")) (list "ban" "can" "fan"))

;;isort: (X X-> Bool) (listof X) -> (listof X)
;; my-filter:  
(define (isort pred? lst)
  (local [(define (insert n sl)
            (cond [(empty? sl)(cons n empty)]
                  [(pred? n (first sl))(cons n sl)] 
                  [else (cons (first sl)(insert n (rest sl)))]))]
                
  (cond [(empty? lst) empty]
        [else (insert (first lst)(isort pred? (rest lst)))])))


;;----------------------W20 lecture 18 clicker question 3---------------
(filter (local[(define x 5)(define (gt x)(< 3 x))] gt) '(1 2 3 4 5 6 7 8 9 10)) ;;the predicate here is the result of a local which produces a function
                                                                                ;; note only the x within the scope of (define (gt x) has meaning


;;----------------------Extended Example (37/42)---------------------
(define-struct point (x y))
;;where a point is (make-point Num Num)

;;we may simulate such with a function:
;; (mk-point x y) produces a "structure" representing (x,y), where we have selector point-x point-y and point?
;; mk-point: Num Num ->
(define (mk-point x y)
  (local [(define (symbol-to-value s)
            (cond [(symbol=? s 'x) x]
                  [(symbol=? s 'y) y]))]
            symbol-to-value))

(define p1 (mk-point 3 4))
;=> (define p1 (local [(define (symbol-to-value s)
;                        (cond [(symbol=? 'x) 3]
;                              [(symbol=? 'y) 4]))]
;                        symbol-to-value))
;
;;;now the local operation: rename "symbol-to-value" and lift it out
;=> (define (symbol-to-value_38 s)
;     (cond [(symbol=? s 'x) 3]
;           [(symbol=? s 'y) 4]))

   ;(define p1 symbol-to-value_38);; now my point p1 is symbol-to-value_38 :p

  ;;(now to get out the x value, we yse (p1 'x):
(p1 'x)
(p1 'y)

;;each time mk-point is called a new "symbol to value"
;; each particular application i.e (mk-point 3 4), is a "copy" of symbol-to value with 3 and 4 substituted for x and y, respectively.

;;-------------------self-check m15_030----------------
;(define (point-checker p)
;        (cond [(num? p)  (p 'x)]
;              [else (error "Bad argument type.")]))


;;----------------------Ex. 9.--------------
;; (create-checker f answers) produces a function that consumes a string and produces a number and produces true if f applied to the argument produces anumber that is in answers

;;create-checker: (Str-> Num)(listof Num) ->(Str-> Bool)

(define (create-checker f answers)
  (local [(define (function str)...)] function))

(define (create-checker f answers)
  (lambda (str) (...)))


;;-----------------Ex.10-------------------------------
(define-struct gnode (key children))
;;a GT (Generalized Tree) is a (make-gnode Nat (listof GT))

;;(tested-gt-sum pred? g-tree) produces the sum of all keys for which pred? produces true
(check-expect (tested-gt-sum odd? (make-gnode 78 (list (make-gnode 81 empty)
                                                       (make-gnode 66 empty)
                                                       (make-gnode 48 (list (make-gnode 37 empty)
                                                                            (make-gnode 12 empty)))
                                                       (make-gnode 11 empty)))) 129)

(define (tested-gt-sum pred? g-tree)
  (local [;;(list-processor children-list) produces the sum of all 
          (define (list-processor children-list)
            (cond [(empty? children-list) 0]
                  [else(+ (tested-gt-sum pred?(first children-list))
                          (list-processor (rest children-list)))]))]

    (cond [(empty? g-tree) 0]
          [(pred? (gnode-key g-tree))(+(gnode-key g-tree)
                                       (list-processor (gnode-children g-tree)))]
          [else (list-processor (gnode-children g-tree))])))




