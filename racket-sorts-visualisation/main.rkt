#lang racket

;; Requires
(require racket/gui/base)

;; Global defines
(define WIDTH 600)
(define HEIGHT 600)

(define MAX 50)
(define MIN 1)
(define NUM 25)

(define SLEEP .02)
(define COLOR "green")

(define the-vec (list->vector (build-list NUM (λ (_x) (+ (random (- MAX MIN)) MIN)))))
(define current 0)
;; Functions

(define (draw-vector vec dc)
  (define i 0)
  (define w (/ WIDTH (vector-length vec)))
  (define h (/ (- HEIGHT 10) MAX))
  (vector-map (λ (elem)
                (if (= i current) 
                    (send dc set-brush (new brush% (color COLOR)))
                    (send dc set-brush (new brush% (color "blue"))))
                (send dc draw-rectangle (* w i) (- HEIGHT (* h elem)) (- w 1) (* h elem))
                (set! i (add1 i)))
              vec))
   
;; Subclasses
(define my-canvas%
  (class canvas%
    (inherit get-dc)
    (super-new)
    
    (define/override (on-paint)
      (send (get-dc) suspend-flush)
      (draw-vector the-vec (get-dc))
      (send (get-dc) resume-flush))))
    
;; Main
(define frame (new frame% (label "Sorts")
                   (width WIDTH)
                   (height HEIGHT)))

(define canvas (new my-canvas% (parent frame)))
(define dc (send canvas get-dc))

(send frame show #t)

(define (bubble-sort name)
  (define swapped #f)
  (define v name)
  (let loop ()
    (define i 0)
    (define temp 0)
    (set! i 0)
    (set! swapped #f)
    (let inner ()
      (set! current i)
      (send canvas refresh-now)
      (sleep SLEEP)
      (cond ((> (vector-ref v i) (vector-ref v (+ i 1)))
          (set! temp (vector-ref v i))
          (vector-set! v i (vector-ref v (+ i 1)))
          (vector-set! v (+ i 1) temp)
          (set! swapped #t)))
      (set! i (add1 i))
      (when (< i (- (vector-length v) 1)) (inner)))
    (when swapped (loop))))

;;the-vec
(bubble-sort the-vec)