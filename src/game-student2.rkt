;;;; 弾の追加
#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;;;; 定数を定義

;; ゲーム画面のサイズ
(define SCENE-SIZE_X 400)
(define SCENE-SIZE_Y 200)
;;ゴールテープの座標
(define GOAL_X 550)

;; 背景
(define SCENE (place-image
               (rectangle (* 2 SCENE-SIZE_X) 5 "solid" "white")
               0 125
               (place-image
                (rectangle (* 2 SCENE-SIZE_X) 5 "solid" "white")
                0 175
                (place-image
                 (rectangle (* 2 SCENE-SIZE_X) 5 "solid" "white")
                 0 150
                 (place-image
                  (rectangle (* 2 SCENE-SIZE_X) 50 "solid" "red")
                  0 150
                  (empty-scene SCENE-SIZE_X SCENE-SIZE_Y "green"))))))
;; 自機
(define ME (place-image
            (circle 10 "outline" "black")
            15 15
            (place-image
             (circle 10 "solid" "white")
             15 15
             (place-image
              (triangle 30 "outline" "black")
              15 30
              (place-image
               (triangle 30 "solid" "white")
               15 30
               (rectangle 30 40 0 "red"))))))
;; ゴール
(define GOAL  (place-image(rectangle 11 61 "outline" "black")
                          6 31
                          (rectangle 12 62 "solid" "white")))
;;敵
(define ENEMY (place-image
               (circle 10 "solid" "black")
               15 15(place-image(triangle 30 "solid" "black")
                                15 30
                                (rectangle 30 40 0 "red"))))


;;;; データの形を定義
(define (me x push) (list x push))
(define (enemy x s l) (list x s l))
(define (goal x) x)
(define (goal-x m) m)

(define (world me-x goal enemy) (list me-x goal enemy))
(define (get-me w) (car w))
(define (get-goal w) (cadr w))
(define (get-enemy w) (caddr w))

(define (get-me-x m) (car m))
(define (get-me-push m) (cadr m))

(define (get-en-x m) (car m))
(define (get-en-velo m) (cadr m))
(define (get-en-level m) (caddr m))

(define m (goal 100))
(define e (enemy 0 2 1))
(define w (world 200 m e))
(define w2 (world 200 "none" e))

;;;; 描画処理
;;自機
(define (place-me me-x s)
  (place-image ME me-x 150
               s))
;;ゴールテープ
(define (place-goal w s)
  (place-image GOAL (- (get-goal w) (get-me-x(get-me w))) 150 s))
;;敵
(define (place-enemy en-x s)
  (place-image ENEMY en-x 130
               s))
;;描画本体
(define (draw-scene w)
  (place-image (text (string-append "level:" (number->string(get-en-level(get-enemy w)))) 16 "black")
               30 10
               (place-image
                (text (cond ((eq? (get-me-push(get-me w)) "right") "RIGHT")
                            ((eq? (get-me-push(get-me w)) "left") "LEFT")
                            ((eq? (get-me-push(get-me w)) "win") "GOAL!! press:R")
                            ((eq? (get-me-push(get-me w)) "lose") "LOSE!! press:R")
                            (else " "))
                      32 "black")
                (/ SCENE-SIZE_X 2)  (/ SCENE-SIZE_Y 2)
                (place-goal w
                            (place-me (get-me-x(get-me w))
                          
                                      (place-enemy (get-en-x(get-enemy w)) SCENE))))))

;;;; 世界の状態を更新する処理
(define (next w)
  (world (cond
           ((>= (get-me-x(get-me w)) (- (get-goal w) (get-me-x(get-me w))))
            (me (get-me-x(get-me w)) "win"))
           ((>= (get-en-x(get-enemy w)) (- (get-goal w) (get-me-x(get-me w))))
            (me (get-me-x(get-me w)) "lose"))
           (else (get-me w))
           )
         (get-goal w)
         (enemy (if(not(or (eq? (get-me-push(get-me w)) "win") (eq? (get-me-push(get-me w)) "lose"))) (+ (get-en-x(get-enemy w)) (get-en-velo (get-enemy w))) (get-en-x(get-enemy w)))
                (get-en-velo (get-enemy w)) (get-en-level (get-enemy w))) 
         ))

;;;; キーボード入力を処理
(define (control w k)
  (cond ((eq? (get-me-push(get-me w)) "right") (cond ((string=? k "left")
                                                      (world (me (- (get-me-x(get-me w)) 2) (get-me-push(get-me w)))
                                                             (get-goal w) (get-enemy w)))
                                                     ((string=? k "right")
                                                      (world (me (+ (get-me-x(get-me w)) 5) "left")
                                                             (get-goal w) (get-enemy w)))
                                                     (else w)))
        ((eq? (get-me-push(get-me w)) "left") (cond ((string=? k "left")
                                                     (world (me (+ (get-me-x(get-me w)) 5) "right")
                                                            (get-goal w) (get-enemy w)))
                                                    ((string=? k "right")
                                                     (world (me (- (get-me-x(get-me w)) 2) (get-me-push(get-me w)))
                                                            (get-goal w) (get-enemy w)))
                                                    (else w)))
        ((eq? (get-me-push(get-me w)) "win")(cond ((string=? k "r")
                                                   (world (me 100 "right")
                                                          (get-goal w)
                                                          (enemy 0 (+ (get-en-velo (get-enemy w)) 0.5) (+ (get-en-level (get-enemy w)) 1))))
                                                  (else w)
                                                  ))
        ((eq? (get-me-push(get-me w)) "lose")(cond ((string=? k "r")
                                                    (world (me 100 "right")
                                                           (get-goal w)
                                                           (enemy 0 1.5 1)))
                                                   (else w)
                                                   ))
        ))

;;;; アプリケーションの実行を開始
(define (test)
  (big-bang (world (me 100 "right") GOAL_X (enemy 0 1.5 1))
    (to-draw draw-scene)
    (on-tick next)
    (on-key control)))