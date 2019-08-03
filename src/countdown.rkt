#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define (draw-scene x)
  (place-image (text (number->string x) 64 "black")
               100 100
               (empty-scene 200 200 "white")))

(define (control x k)
  (- x 1))

(define (next x)
  (- x 1))

(define (start)
  (big-bang 10
    [to-draw draw-scene]
    [on-key control]))
