#lang racket
(require rackunit)


;Define loot primitive
(struct loot (name weight value))
(define (loot-value-density item)
  (/ (loot-value item) (loot-weight item)))

(define gold-ring (loot "gold ring" 3 100))
(check-equal? (loot-name gold-ring) "gold ring")
(check-equal? (loot-weight gold-ring) 3)
(check-equal? (loot-value gold-ring) 100)
(check-equal? (loot-value-density gold-ring) 100/3)

;sorting items by value/weight
(define (sort-by-value-per-weight items)
  (sort items (lambda (first second)
                (>= (loot-value-density first) (loot-value-density second)))))

(check-equal? (< 1/10 2/10) #t)
(define test-treasure-chest (list (loot "tree stump" 100 2)
                       (loot "bread" 10 20)
                       (loot "gold ring" 1 100)
                       (loot "stone" 20 1)))
(check-equal? (loot-name (first (sort-by-value-per-weight test-treasure-chest))) "gold ring")
(check-equal? (loot-weight (second (sort-by-value-per-weight test-treasure-chest))) 10)
(check-equal? (loot-value (last (sort-by-value-per-weight test-treasure-chest))) 2)

;greedy part with simple items
(define (grab-recur things total-limit running-total thing-value-selector)
  (if (null? things)
      `()
      (let ([current-value (thing-value-selector (first things))]
            [remaining-numbers (cdr things)])
        (if (> (+  running-total current-value) total-limit)
            (grab-recur remaining-numbers total-limit running-total thing-value-selector)
            (cons (first things) (grab-recur remaining-numbers total-limit (+ running-total current-value) thing-value-selector))))))
    
(define (grab-num list total-limit) (grab-recur list total-limit 0 identity))

(define some-numbers `(1 9 4 5 10 3 14))
(check-equal? (grab-num some-numbers 10) `(1 9))
(check-equal? (grab-num some-numbers 3) `(1))
(check-equal? (grab-num some-numbers 5) `(1 4))
(check-equal? (grab-num some-numbers 8) `(1 4 3))
(check-equal? (grab-num some-numbers 16) `(1 9 4))
(check-equal? (grab-num some-numbers 100) `(1 9 4 5 10 3 14))

;now with inventory items
(define (take-maximum-loot available-loot weight-limit)
  (let ([loot-by-value-per-weight (sort-by-value-per-weight available-loot)])
    (grab-recur loot-by-value-per-weight weight-limit 0 loot-weight)))

(define available-loot (list (loot "Aged Brie" 3 18)
                    (loot "Elixir of the Mongoose" 2 50)
                    (loot "+5 Dexterity Vest" 1 1)
                    (loot "Sulfuras, Hand of Ragnaros" 10 1)
                    (loot "Backstage passes to a TAFKAL80ETC concert" 1 100)
                    (loot "Conjured Mana Cake" 100 1)))

(check-equal?
 (map loot-name (take-maximum-loot available-loot 10))
 `("Backstage passes to a TAFKAL80ETC concert"
   "Elixir of the Mongoose"
   "Aged Brie"
   "+5 Dexterity Vest"))
                    
                    
                    