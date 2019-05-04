#lang racket
(require rackunit)

(define hand-definitions
  `(("Straight flush" ((any any) (any equal) (any equal) (any equal) (any equal)))
    ("Four of a kind" ((any any) (incremented any) (incremented any) (incremented any)))
    ("Full house" ((any any) (equal any) (equal any)) ((any any) (equal any)))
    ("Flush" (any any) (any equal) (any equal) (any equal) (any equal))
    ("Straight" (any any) (incremented any) (incremented any) (incremented any) (incremented any))
    ("Three of a kind" (any any) (equal any) (equal any))
    ("Two pair" ((any any) (equal any)) ((any any) (equal any)))
    ("One pair" ((any any) (equal any)))
    ("High card" ((any any) (any any) (any any) (any any) (any any)))))


(define (match-rec hand-sub-def cards)
  (if (empty? hand-sub-def)
      cards
      (append cards (match-rec (second ))

(define (match hand-def cards)
  (if (empty? (first hand-def))
      `()
;      `()))
      (let ([cards-match (match-rec (first hand-def) cards)])
        (append `(first cards-match) (match (cdr hand-def) (cdr cards-match))))))
   
(define (is-match? match)
  (not (empty? (first match))))

(define (identify hand)
  (first (filter is-match? (map
                     (lambda (hand-def) (list (first hand-def) (match hand-def hand)))
                     hand-definitions))))

;rank: input ((any any) (incremented any) (incremented any) (incremented any)) (2 2 4 2 2)
;ranking = `("Four of a kind" ((2 2 2 2))

;rank: input (((any any) (equal any)) ((any any) (equal any))) (2 2 4 3 3)
;ranking = `("Two pair" ((2 2) (3 3))

;tests
(check-equal? (match-rec `(any any) `((2 `Hearts))) `(((2 `Hearts)) `()))
;(append `(()) `((1 2)))
;(append `((5 6)) (append `((3 4)) `((1 2))))
;(check-false (has-rank? `(() "Some hand")))
;(check-true (has-rank? `((1 2) "Some hand")))
;(check-equal? (rank `("" ((any any))) `(1 2 3)) (3))
;(check-eq? (identify `((2 Hearts) (2 Hearts))) "One pair")
;(check-eq? (identify `((2 Hearts) (2 Hearts) (2 Hearts))) "Three of a kind")



          