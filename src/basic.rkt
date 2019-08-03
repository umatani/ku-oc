;; おまじない(プログラムの先頭には必ずこう書く)
#lang racket
(require 2htdp/image)
(require 2htdp/universe)


(define (f x) (+ (* 3 x) 2))
(define (g x y) (- (+ (* x y) x) 2))

(define (absolute x)
  (cond [(>= x 0) x]
        [else (- x)]))

;; 読みにくい
(/ (* 4 (* 3.141592 (* (* 3 3) 3))) 3)
;; 読みやすい
(/ (* 4 (* 3.141592
           (* 3 3 3)))
   3)
;; 読み間違える
(/ (* 4 (* 3.141592
   (* 3 3 3)))
      3)


(define r 3)
(define pi 3.141592)
(define vol (/ (* 4
                  (* pi
                     (* r r r)))
               3))
(define surface (* 4 (* pi (* r r))))

(define (volume r)
  (/ (* 4
        (* pi
           (* r r r)))
     3))

"red"
(string-append "This is a "
               "Racket code.")
(string-length "Hello")

(triangle 50 "solid" "red")

(place-image (triangle 20 "solid" "yellow")
             25 25
             (triangle 50 "solid" "red"))


(define x (list 1 "black"))

(first x)

(second x)

(define (absolute2 x)
  (cond [(< x 0) (- x)]
        [else x]))

(define (zero f z) z)
(define (one f z) (f z))
(define (two f z) (f (f z)))

(define (plus m n)
  (define (k f z) (m f (n f z)))
  k)




(newline)(newline)(newline)(newline)(newline)
(newline)(newline)(newline)(newline)(newline)
(newline)(newline)(newline)(newline)(newline)
