;;;; 複数の壁を配置するバージョン
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
(define (world me-x bs ms) (list me-x bs ms))
(define (brick x y v) (list x y v))
(define (missile x y) (list x y))

(define (get-me-x w) (first w))
(define (get-bricks w) (second w))
(define (get-missiles w) (third w))

(define (brick-x b) (first b))
(define (brick-y b) (second b))
(define (brick-v b) (third b))

(define (missile-x m) (first m))
(define (missile-y m) (second m))

;;;; 描画処理
(define (place-me me-x s)
  (place-image ME me-x (- SCENE-SIZE
                          (/ (image-height ME) 2))
               s))

(define (place-brick b s)
  (place-image BRICK (brick-x b) (brick-y b) s))

(define (place-bricks bs s)
  (cond [(empty? bs) s]
        [else (place-brick (first bs)
                           (place-bricks (rest bs) s))]))

(define (place-missile m s)
  (place-image MISSILE (missile-x m) (missile-y m) s))

(define (place-missiles ms s)
  (cond [(empty? ms) s]
        [else (place-missile (first ms)
                             (place-missiles (rest ms) s))]))

(define (draw-scene w)
  (place-me (get-me-x w)
    (place-bricks (get-bricks w)
      (place-missiles (get-missiles w) SCENE))))

;;;; 世界の状態を更新する処理
(define (hit? m b)
  (and (< (abs (- (missile-x m) (brick-x b)))
          (/ BRICK-WIDTH 2))
       (< (abs (- (missile-y m) (brick-y b)))
          (/ BRICK-HEIGHT 2))))

(define (move-missile m)
  (missile (missile-x m) (- (missile-y m) 8)))

(define (move-brick b)
  (cond [(> (brick-x b) (- SCENE-SIZE (/ BRICK-WIDTH 2)))
         (brick (- SCENE-SIZE (/ BRICK-WIDTH 2))
                (brick-y b)
                (- (brick-v b)))]
        [(< (brick-x b) (/ BRICK-WIDTH 2))
         (brick (/ BRICK-WIDTH 2)
                (brick-y b)
                (- (brick-v b)))]
        [else (brick (+ (brick-x b) (brick-v b))
                     (brick-y b)
                     (brick-v b))]))

(define (next w)
  (define (missile-alive? m)
    (define (missile-hit-bricks? bs)
      (cond [(empty? bs) #false]
            [(hit? m (first bs)) #true]
            [else (missile-hit-bricks? (rest bs))]))
    (and (not (missile-hit-bricks? (get-bricks w)))
         (>= (missile-y m) 0)))
  (define (brick-alive? b)
    (define (missiles-no-hit-brick? ms)
      (cond [(empty? ms) #true]
            [(hit? (first ms) b) #false]
            [else (missiles-no-hit-brick? (rest ms))]))
    (missiles-no-hit-brick? (get-missiles w)))

  (world (get-me-x w)
         (map move-brick
              (filter brick-alive?
                      (get-bricks w)))
         (map move-missile
              (filter missile-alive?
                      (get-missiles w)))))

;;;; キーボード入力を処理
(define (control w k)
  (cond [(string=? k "left")
         (world (- (get-me-x w) 5)
                (get-bricks w)
                (get-missiles w))]
        [(string=? k "right")
         (world (+ (get-me-x w) 5)
                (get-bricks w)
                (get-missiles w))]
        [(string=? k " ")
         (world (get-me-x w)
                (get-bricks w)
                (cons (missile (get-me-x w)
                               (- SCENE-SIZE (image-height ME)))
                      (get-missiles w)))]
        [else w]))

;;;; アプリケーションの実行を開始
(define (start me-x)
  (big-bang (world me-x
                   (list (brick  50 16 -5)
                         (brick 150 48  8)
                         (brick 100 80 -8)
                         (brick 200 112 5))
                   (list))
    [to-draw draw-scene]
    [on-tick next]
    [on-key control]))
