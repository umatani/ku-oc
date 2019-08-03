;;;; 壁の動作が複雑なバージョン
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
;; 壁の横幅
(define BRICK-WIDTH 100)
;; 壁の縦の高さ
(define BRICK-HEIGHT 30)
;; 壁
(define BRICK (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" "blue"))

;;;; データの形を定義
(define (world me-x b mi) (list me-x b mi))
(define (brick x v) (list x v))
(define (missile x y) (list x y))

(define (get-me-x w) (first w))
(define (get-brick w) (second w))
(define (get-missile w) (third w))

(define (brick-x b) (first b))
(define (brick-v b) (second b))

(define (missile-x m) (first m))
(define (missile-y m) (second m))

;;;; 描画処理
(define (place-me me-x s)
  (place-image ME me-x (- SCENE-SIZE
                          (/ (image-height ME) 2))
               s))

(define (place-brick b s)
  (cond [(string? b) s]
        [else (place-image BRICK (brick-x b) 112 s)]))

(define (place-missile m s)
  (cond [(string? m) s]
        [else (place-image MISSILE
                           (missile-x m) (missile-y m)
                           s)]))

(define (draw-scene w)
  (place-me (get-me-x w)
    (place-brick (get-brick w)
      (place-missile (get-missile w) SCENE))))

;;;; 世界の状態を更新する処理
(define (hit? m b)
  (and (not (string? m))
       (not (string? b))
       (< (abs (- (missile-x m) (brick-x b)))
          (/ BRICK-WIDTH 2))
       (< (abs (- (missile-y m) 112))
          (/ BRICK-HEIGHT 2))))

(define (move-missile m)
  (missile (missile-x m) (- (missile-y m) 8)))

(define (move-brick b)
  (cond [(< (brick-x b) (/ BRICK-WIDTH 2))
         (brick (/ BRICK-WIDTH 2)
                (- (brick-v b)))]
        [(> (brick-x b) (- SCENE-SIZE (/ BRICK-WIDTH 2)))
         (brick (- SCENE-SIZE (/ BRICK-WIDTH 2))
                (- (brick-v b)))]
        [else (brick (+ (brick-x b) (brick-v b))
                     (brick-v b))]))

(define (missile-alive? m b)
  (and (not (hit? m b)) (>= (missile-y m) 0)))

(define (next w)
  (world (get-me-x w)
         (cond [(string? (get-brick w)) "none"]
               [(not (hit? (get-missile w) (get-brick w)))
                (move-brick (get-brick w))]
               [else "none"])
         (cond [(string? (get-missile w)) "none"]
               [(missile-alive? (get-missile w) (get-brick w))
                (move-missile (get-missile w))]
               [else "none"])))

;;;; キーボード入力を処理
(define (control w k)
  (cond [(string=? k "left")
         (world (- (get-me-x w) 5)
                (get-brick w)
                (get-missile w))]
        [(string=? k "right")
         (world (+ (get-me-x w) 5)
                (get-brick w)
                (get-missile w))]
        [(string=? k " ")
         (world (get-me-x w)
                (get-brick w)
                (cond [(string? (get-missile w))
                       (missile (get-me-x w)
                                (- SCENE-SIZE (image-height ME)))]
                      [else (get-missile w)]))]
        [else w]))

;;;; アプリケーションの実行を開始
(define (start me-x)
  (big-bang (world me-x
                   (brick 200 5)
                   "none")
    [to-draw draw-scene]
    [on-tick next]
    [on-key control]))
