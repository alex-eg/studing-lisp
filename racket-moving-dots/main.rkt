;;#lang racket

;; Requires
(require racket/gui/base)

;; Structs
(define-struct moving-dot (position velocity))

(define-struct position (x y))

(define-struct velocity (x y))

;; Global defines
(define WIDTH 600)
(define HEIGHT 600)
(define MAX-SPEED 10)
(define MIN-SPEED 1)

;; Functions
(define (make-random-dot)
  (make-moving-dot (make-position (+ 1 (random WIDTH))
                                  (+ 1 (random HEIGHT)))
                   (make-velocity (+ MIN-SPEED (random (- MAX-SPEED MIN-SPEED)))
                                  (+ MIN-SPEED (random (- MAX-SPEED MIN-SPEED))))))

(define (update-dot dot)
  (let
      ((dot-x (position-x (moving-dot-position dot)))
       (dot-y (position-y (moving-dot-position dot)))
       (dot-dx (velocity-x (moving-dot-velocity dot)))
       (dot-dy (velocity-y (moving-dot-velocity dot))))
    (let
        ((new-dx (if (or (> (+ dot-x dot-dx) WIDTH)
                         (< (+ dot-x dot-dx) 0))
                     (- dot-dx)
                     dot-dx))
         (new-dy (if (or (> (+ dot-y dot-dy) HEIGHT)
                         (< (+ dot-y dot-dy) 0))
                     (- dot-dy)
                     dot-dy)))
      (make-moving-dot (make-position (+ dot-x new-dx)
                                      (+ dot-y new-dy))
                       (make-velocity new-dx new-dy)))))

(define (make-dot-list n)
  (define (make-dot-list-acc n acc)
    (if (= n 0) acc
        (make-dot-list-acc (- n 1) (cons (make-random-dot) acc))))
  (make-dot-list-acc n empty))

(define (update-dots dot-list)
  (map update-dot dot-list))

;; Main
(define dots (make-dot-list 20))

(define frame (new frame%
                   (label "Moving dots")
                   (width WIDTH)
                   (height HEIGHT)))

(define canvas (new canvas%
                    (parent frame)
                    (style (list 'border))
                    (paint-callback
                     (λ (canvas dc)
                        (map (λ (dot)
                                (send dc draw-ellipse
                                      (position-x (moving-dot-position dot))
                                      (position-y (moving-dot-position dot))
                                      5 5))
                             dots)
                        (set! dots (update-dots dots))))))

(let ((gray (make-object color%)))
  (send gray set 235 240 255)
  (send canvas set-canvas-background gray))

(send frame show #t)

(let
    ((t (new timer%
             (notify-callback (λ ()
                                 (send canvas refresh))))))
  (send t start 20))
