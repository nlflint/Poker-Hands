#lang racket
(require rackunit)

(define hand-patterns
  `(("Straight flush" ((any any) (incremented equal) (incremented equal) (incremented equal) (incremented equal)))
    ("Four of a kind" ((any any) (equal any) (equal any) (equal any)))
    ("Full house" ((any any) (equal any) (equal any)) ((any any) (equal any)))
    ("Flush" ((any any) (any equal) (any equal) (any equal) (any equal)))
    ("Straight" ((any any) (incremented any) (incremented any) (incremented any) (incremented any)))
    ("Three of a kind" ((any any) (equal any) (equal any)))
    ("Two pair" ((any any) (equal any)) ((any any) (equal any)))
    ("One pair" ((any any) (equal any)))
    ("High card" ((any any) (any any) (any any) (any any) (any any)))))

(define one-pair-definition `("One pair" ((any any) (equal any))))
(define three-of-a-kind `("Three of a kind" ((any any) (equal any) (equal any))))

(define value first)
(define suit second)


(define (make-value-predicate value-definition previous-card)
  (match value-definition
    [`any (lambda (card) #t)]
    [`equal (lambda (card) (eq? (value previous-card) (value card)))]
    [`incremented (lambda (card) (eq? (add1 (value previous-card)) (value card)))]))

(define (make-suit-predicate suit-definition previous-card)
  (match suit-definition
    [`any (lambda (card) #t)]
    [`equal (lambda (card) (eq? (suit previous-card) (suit card)))]))
  

(define (make-card-predicate hand-defintion previous-card)
  (let ([value-predicate (make-value-predicate (first hand-defintion) previous-card)]
        [suit-predicate (make-suit-predicate (second hand-defintion) previous-card)])
    (lambda (card)
      (and
       (value-predicate card)
       (suit-predicate card)))))
   
(define (match-rec card-definitions cards previous-card matched-cards)
  (if (empty? card-definitions)
      (list (list matched-cards cards))
      (let ([next-possible-cards (filter (make-card-predicate (first card-definitions) previous-card) cards)])
        (if (empty? next-possible-cards)
            (list (list `() (append matched-cards cards)))
            (map (lambda (card) (first-or-default (match-rec (cdr card-definitions) (remove card cards) card (cons card matched-cards)))) next-possible-cards)))))

(define (first-or-default matches)
  (cond
    [(empty? matches) `()]
    [(empty? (first matches)) (first-or-default (cdr matches))]
    [else (first matches)]))

(define (match-hand hand-definitions hand)
  (first-or-default (match-rec hand-definitions (cards hand) `() `())))

(define (identify-hand-rec pattern hand)
  (if (empty? pattern)
      null
      (let ([match-result (match-hand (first pattern) hand)])
        
         (let ([rec (identify-hand-rec (cdr pattern) (make-hand (name hand) (second match-result)))]
               [matches (first match-result)])
           (cons matches rec)))))

(define and-l (lambda x 
    (if (null? x)
        #t
        (if (car x) (apply and-l (cdr x)) #f))))

(define (is-match? match-result)
  (let* ([pattern-matches (second match-result)]
         [empty-projection (map pair? pattern-matches)]
         [result (apply and-l empty-projection)]
         [some-list (list 1 2 3)])
  result))
  
(define (identify-hand hand)
  (first-or-default
   (map (lambda (pattern)
          (let
            ([match-result (list (first pattern) (identify-hand-rec (cdr pattern) hand))])
            (cond
              [(is-match? match-result) (first match-result)]
              [else null])))
        hand-patterns)))

(define make-hand cons)
(define name first)
(define cards cdr)


;tests
(check-true (eq? `any (quote any)))
(check-true ((make-value-predicate `any `(2 Hearts)) `(3 Clubs)))
(check-true ((make-suit-predicate `any `(2 Hearts)) `(3 Clubs)))
(check-true ((make-card-predicate `(any any) `(10 Spades)) `(5 Diamonds)))
(check-false (apply and-l `(#f #f #t)))
(check-false (apply and-l `(#f #t)))
(check-true (apply and-l `(#t #t)))
;(check-equal?
;(match-hand (second one-pair-definition) (make-hand "Nate" `((2 Hearts) (3 Hearts) (2 Clubs))))
; `(((2 Clubs) (2 Hearts)) ((3 Hearts))))
;(check-equal? (match-hand (second one-pair-definition) (make-hand "nate" `((10 Hearts) (9 Hearts) (6 Clubs)))) `(() ((10 Hearts) (9 Hearts) (6 Clubs))))
;(match-hand (second one-pair-definition) (make-hand "nate" `((2 Hearts) (3 Hearts) (2 Clubs) (2 Diamonds))))
;(match-hand (second three-of-a-kind) (make-hand "nate" `((2 Hearts) (3 Hearts) (2 Clubs) (2 Diamonds))))
;(match-hand (second three-of-a-kind) (make-hand "nate" `((2 Hearts) (3 Hearts) (2 Clubs) (2 Diamonds))))
(match-hand (second (first hand-patterns)) (make-hand "nate" `((6 Hearts) (3 Hearts) (4 Hearts) (5 Hearts) (2 Hearts))))
;(identify-hand (make-hand "nate" `((2 Hearts) (2 Hearts))))
;(identify-hand (make-hand "nate" `((2 Hearts) (2 Hearts) (2 Spades) (3 Hearts))))
;(identify-hand (make-hand "nate" `((14 Hearts) (14 Hearts) (14 Spades) (14 Hearts))))
;(identify-hand (make-hand "joe" `((13 Hearts) (13 Hearts) (14 Spades) (14 Hearts))))
;(identify-hand (make-hand "joe" `((13 Hearts) (12 Hearts) (11 Hearts) (2 Hearts) (6 Hearts))));
;(identify-hand (make-hand "joe" `((13 Hearts) (12 Hearts) (11 Hearts) (10 Hearts) (9 Hearts))))
;(identify-hand (make-hand "dude" `((13 Hearts) (13 Hearts) (14 Spades) (14 Hearts) (14 Hearts))))

;(define two-pair-pattern `("Two pair" ((any any) (equal any)) ((any any) (equal any))))
;(identify-hand-rec (cdr two-pair-pattern) (make-hand "nates" `((2 Hearts)(2 Clubs))))

;(define full-house-pattern `("Full house" ((any any) (equal any) (equal any)) ((any any) (equal any))))
;(identify-hand-rec (cdr full-house-pattern) (make-hand "nates" `((2 Hearts)(2 Clubs))))

;(identify-hand-rec (cdr (first hand-patterns)) (make-hand "nates" `((6 Hearts) (3 Hearts) (4 Hearts) (5 Hearts) (2 Hearts))))

;(append `((2 Hearts) (2 Clubs)) null)
;(check-equal?
; (first (match-it one-pair-definition `((2 Hearts) (3 Hearts) (2 Clubs)) `())
; `((2 Hearts) (2 Clubs))))


;(check-equal? (match-rec `(any any) `((2 `Hearts))) `(((2 `Hearts)) `()))
;(append `(()) `((1 2)))
;(append `((5 6)) (append `((3 4)) `((1 2))))
;(check-false (has-rank? `(() "Some hand")))
;(check-true (has-rank? `((1 2) "Some hand")))
;(check-equal? (rank `("" ((any any))) `(1 2 3)) (3))
;(check-eq? (identify `((2 Hearts) (2 Hearts))) "One pair")
;(check-eq? (identify `((2 Hearts) (2 Hearts) (2 Hearts))) "Three of a kind")



          