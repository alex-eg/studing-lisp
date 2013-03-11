#lang racket

;; Requires
(require racket/gui/base)

;; Global defines
(define WIDTH 600)
(define HEIGHT 600)

(define MAX 10)
(define MIN 0)

;; Main
#|(define frame (new frame% (label "Sorts")
                   (width WIDTH)
                   (height HEIGHT)))

(define dc (send (new canvas% (parent frame)
                    (paint-callback (λ (canvas dc) (void)))) 
                 get-dc))

(send frame show #t)|#

(define (bubble-sort list)
  (define swapped #f)
  (define v (list->vector list))
  (let loop ()
    (define i 0)
    (define temp 0)
    (set! i 0)
    (set! swapped #f)
    (let inner ()
      (cond ((> (vector-ref v i) (vector-ref v (+ i 1)))
          (set! temp (vector-ref v i))
          (vector-set! v i (vector-ref v (+ i 1)))
          (vector-set! v (+ i 1) temp)
          (set! swapped #t)))
      (set! i (add1 i))
      (when (< i (- (length list) 1)) (inner)))
    (when swapped (loop)))
  v)

(define l (build-list 10 (λ (_x)(+ MIN (random MAX)))))
(display l)
(display (bubble-sort l))

