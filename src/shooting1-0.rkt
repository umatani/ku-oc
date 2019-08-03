;;;; 弾の追加
#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;;;; 定数を定義

;; ゲーム画面のサイズ
(define SCENE-SIZE 400)
;; ゲーム画面
(define SCENE (empty-scene SCENE-SIZE SCENE-SIZE "white"))
;; 自機
(define ME (triangle 30 "solid" "black"))
;; 弾
(define MISSILE (circle 10 "solid" "red"))

;;;; データの形を定義
(define (missile x y) (list x y))
(define (missile-x m) (first m))
(define (missile-y m) (second m))

(define (world me-x mi) (list me-x mi))
(define (get-me-x w) (first w))
(define (get-missile w) (second w))

(define m (missile 100 250))
(define w (world 200 m))
(define w2 (world 200 "none"))

;;;; 描画処理
(define (place-me me-x s)
  (place-image ME me-x (- SCENE-SIZE
                          (/ (image-height ME) 2))
               s))

(define (place-missile m s)
  (cond [(string? m) s]
        [else (place-image MISSILE (missile-x m) (missile-y m) s)]))

(define (draw-scene w)
  (place-me (get-me-x w)
    (place-missile (get-missile w) SCENE)))

;;;; キーボード入力を処理
(define (control w k)
  (cond
    [(string=? k "left")
     (world (- (get-me-x w) 5)
            (get-missile w))]
    [(string=? k "right")
     (world (+ (get-me-x w) 5)
            (get-missile w))]
    [(string=? k " ")
     (world (get-me-x w)
            (cond [(string? (get-missile w))
                   (missile (get-me-x w)
                            (- SCENE-SIZE (image-height ME)))]
                  [else (get-missile w)]))]
    [else w]))

;;;; アプリケーションの実行を開始
(define (start me-x)
  (big-bang (world me-x "none")
    [to-draw draw-scene]
    [on-key control]))
