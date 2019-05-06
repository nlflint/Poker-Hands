#lang racket
(require rackunit)

(define hand-definitions
  `(("Straight flush" ((any any) (incremented equal) (incremented equal) (incremented equal) (incremented equal)))
    ("Four of a kind" ((any any) (equal any) (equal any) (equal any)))
    ("Full house" ((any any) (equal any) (equal any)) ((any any) (equal any)))
    ("Flush" (any any) (any equal) (any equal) (any equal) (any equal))
    ("Straight" (any any) (incremented any) (incremented any) (incremented any) (incremented any))
    ("Three of a kind" ((any any) (equal any) (equal any)))
    ("Two pair" ((any any) (equal any)) ((any any) (equal any)))
    ("One pair" ((any any) (equal any)))
    ("High card" ((any any) (any any) (any any) (any any) (any any)))))




(define one-pair-definition `("One pair" ((any any) (equal any))))
(define three-of-a-kind `("Three of a kind" ((any any) (equal any) (equal any))))


(define (make-value-predicate value-definition previous-card)
  (match value-definition
    [`any (lambda (card) #t)]
    [`equal (lambda (card) (eq? (first previous-card) (first card)))]))

(define (make-suit-predicate suit-definition previous-card)
  (match suit-definition
    [(quote any) (lambda (card) #t)]))
  

(define (make-card-predicate hand-defintion previous-card)
  (let ([value-predicate (make-value-predicate (first hand-defintion) previous-card)]
        [suit-predicate (make-suit-predicate (second hand-defintion) previous-card)])
    (lambda (card)
      (and
       (value-predicate card)
       (suit-predicate card)))))
    
    
    

(define (match-rec card-definitions cards previous-card matched-cards)
  (if (empty? card-definitions)
      (list matched-cards cards)
      (map
       (lambda (card) (first-or-default (match-rec (cdr card-definitions) (remove card cards) card (cons card matched-cards))))
       (filter (make-card-predicate (first card-definitions) previous-card) cards))))

(define (first-or-default matches)
  (cond
    [(empty? matches) `()]
    [(empty? (first matches)) (first-or-default (cdr matches))]
    [else (first matches)]))
      

(null? (cdr (list 1)))

(define (matches-hand? card-definitions cards)
  (first-or-default (match-rec card-definitions cards `() `())))

;tests
(check-true (eq? `any (quote any)))
(check-true ((make-value-predicate `any `(2 Hearts)) `(3 Clubs)))
(check-true ((make-suit-predicate `any `(2 Hearts)) `(3 Clubs)))
(check-true ((make-card-predicate `(any any) `(10 Spades)) `(5 Diamonds)))
(matches-hand? (second one-pair-definition) `((2 Hearts) (3 Hearts) (2 Clubs)))
(matches-hand? (second one-pair-definition) `((10 Hearts) (9 Hearts) (6 Clubs)))
(matches-hand? (second one-pair-definition) `((2 Hearts) (3 Hearts) (2 Clubs) (2 Diamonds)))
(matches-hand? (second three-of-a-kind) `((2 Hearts) (3 Hearts) (2 Clubs) (2 Diamonds)))

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



          