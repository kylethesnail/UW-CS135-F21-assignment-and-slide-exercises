;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname QASessionDec4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;--------------------------Find-Path-----------------------------------------
;;Starting from v1
;;What is a graph: similar to a tree, but we have a node and a bunch of edges
(define g1
  '((A (C D E))
    (B (E J))
    (C())
    (D(F J))
    (E(K))
    (F(K H))
    (H())
    (J(H))
    (K())))

;;find-path-v1
;;two functions: 1. handles the node itself 2. handles the nbrs of the node (mutual recursion happening here)
;; if a list is formed, keeps cons elements on to the list

(define (find-path orig dest g)
  (cond [(symbol=? orig dest)(list dest)] ;;base case: the destination is right at where we start with
        [else(local [(define nbrs (neighbours orig g)) ;; call our neighbours if we are not already at where we want to be
                     (define maybe-path? (find-path/list nbrs dest g))]
               (cond [(false? maybe-path?) false] ;; no path from this neighbour to the destination
                       [else (cons orig maybe-path?)]))])) ;; if maybe-path yields anything other than false, we have ourselves a path, we cons the origin on the path recursively


;;(find-path/list nbrs dest g) produces a path from an element of nbrs to dest in g, if one exists
;;find-path/list: (listof Node) Node Graph -> (anyof (listof Node) false)
(define (find-path/list nbrs dest g);;recurse on the list of neighbours provided
  (cond [(empty? nbrs) false]  ;; no path found among all neighbours looked 
        [else (local [(define maybe-path? (find-path (first nbrs) dest g))] ;; check and see if there is a path from the first of the "neighbours" to the destination(Mutual-Recursion)
                 (cond [(false? maybe-path?);; no path from the first neighbour to the destination
                        (find-path/list (rest nbrs) dest g)]
                       [else maybe-path?]))]))

;;find-path-v2
;;v1 will fail when there is cycle involved
;;we introduce an accumulative recursion which keeps track of the nodes which we have already visited
;;Use accumulative recusion to solve the problem of find-path possibly not terminating if there are cycles in the graph
;;the accumulator keeps track of a list of visited nodes and we can then avoid visiting a node twice
;;we will just add a check in find-path/list

;;>>find-path/list-v2: not much changed 
;; find-path/list: (listof Node) Node Graph (listof Node) -> (anyof (listof Node) galse)
(define (find-path/list-v2 nbrs dest g visited)
  (cond [(empty? nbrs) false]
        [(member? (first nbrs) visited) ;;if the firstneighbour is already visited
         (find-path/list (rest nbrs) dest g visited)] ;; simply skip to the second neighbour
        [else (local [(define path (find-path/acc-v2 (first nbrs)
                                                  dest g visited))]
                (cond [(false? path)
                       (find-path/list (rest nbrs) dest g visited)]
                      [else path]))]))

;;find-path/acc: Node Node Graph (listof Node) -> (anyof (listof Node) false)
(define (find-path/acc-v2 orig dest g visited)
  (cond [(symbol=? orig dest)(list dest)]
        [else (local [(define nbrs (neighbours orig g))
                      (define path (find-path/list-v2 nbrs dest g
                                                   (cons orig visited)))];; add the node to the answer once you start exploring its neighbouts
                (cond [(false? path) false]
                      [else (cons orig path)]))]))

;;new wrapper function
(define (find-path-v2 orig dest g)
  (find-path/acc-v2 orig dest g '()))

;;find-path-v3
;;in the list function when we want to go to the next neighbour but often times we don't rememebr the nodes we have already visited

(define-struct success (path))
;; a Success is a (make-sucess (listof Node)),a success structure contains a list of nodes that contain a path

(define-struct failure (visited))
;; a Failure is a (make-failure (listof Node)) a failure structure will contain a list of nodes that contain the visited nodes
;; a Result is (anyof failure success)

;;>>find-path/list
;;find-path/list: (listof Node) Node Graph (listof Node) -> Result <<either fail or success>>
(define (find-path/list-v3 nbrs dest g visited)
  (cond [(empty? nbrs)(make-failure visited)]
        [(member? (first nbrs)visited)
         (find-path/list-v3 (rest nbrs) dest g visited)]
        [else (local [(define result (find-path/acc-v3 (first nbrs) ;; we have since renamed the original "path" into result for clarityp
                                                    dest g visited))]
                (cond [(failure? result)
                       (find-path/list-v3 (rest nbrs) dest g
                                          (failure-visited result))]
                      [(success? result) result]))]))

 ;;find-path/acc: Node Node Graph (listof Node) -> Result <<either fail or success>>
 (define (find-path/acc-v3 orig dest g visited)
   (cond [(symbol=? orig dest)(make-success (list dest))]
         [else (local [(define nbrs (neighbours orig g))
                       (define result (find-path/list nbrs dest g (cons orig visited)))]
                 (cond [(failure? result) result]
                       [(success? result) (make-success (cons orig (success-path result)))]))])) ;; extract the path from success-struct,
                                                                                                 ;;cons another element onto it then make it a success again
                                                                                                 ;;extract the path from the new success struct again and add another element
                                                                                                 ;; repeat the process again and again until 

 ;;another wrapper function which hides the original use of failure and success structure
 ;;find-path: Node Node Graph -> (anyof (listof Node) false)
 (define (find-path-v3 orig dest g)
   (local[(define result (find-path/acc-v3 orig dest g empty))]
     (cond [(success? result)(success-path result)]
           [(failure? result) false])))