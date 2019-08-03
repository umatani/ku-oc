;;;; おまじない(プログラムの先頭に必ず書く)
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

;;;; 描画処理
(define (draw-scene me-x)
  (place-image ME
               me-x (- SCENE-SIZE
                       (/ (image-height ME) 2))
               SCENE))

;;;; キーボード入力を処理
(define (control me-x k)
  (cond [(string=? k "left")  (- me-x 5)]
        [(string=? k "right") (+ me-x 5)]
        [else me-x]))

;;;; アプリケーションの実行を開始
(define (start me-x)
  (big-bang me-x
            [to-draw draw-scene]
            [on-key control]))
