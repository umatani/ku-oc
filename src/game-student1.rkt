;;;; 壁の動作が複雑なバージョン
#lang racket
(require 2htdp/image)
(require 2htdp/universe)


;;;; 定数定義
;; 設定
(define SCENESIZE-X 600)
(define SCENESIZE-Y 300)
(define MISSILE-SIZE 6)
(define MISSILE-V 16)
(define READY-TIME 2)

(define FIN-TIME 60)    ;;;;;;;;  この定数を変えるとゲームの時間を変えられます   ;;;;;;;;;;;;

;(define LIFE 3)

;; ゲーム画面
(define SCENE (empty-scene SCENESIZE-X SCENESIZE-Y "black"))
;; 自機グラフィック設定
(define ME (rotate 30 (triangle 30 "solid" "white")))
;; 弾グラフィック設定
(define MISSILE (circle MISSILE-SIZE "solid" "red"))
;; 壁の横幅設定
(define BRICK-WIDTH 30)
;; 壁の縦の高さ設定
(define BRICK-HEIGHT 30)
;; 障害物グラフィック設定
(define BRICK (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" "white"))

;;;; リスト形を定義
(define (world me b mi time score mi-num) (list me b mi time score mi-num));;mi-num=発射されたミサイルの数
(define (me x y) (list x y))
(define (brick x y v) (list x y v))
(define (m-plus a b) (list a b))
(define (missile x y) (list x y))
(define (get-me w) (car w))
(define (get-brick w) (cadr w))
(define (get-missile w) (third w))
(define (get-time w) (fourth w))
(define (get-score w) (fifth w))
(define (get-mi-num w) (sixth w))

(define (brick-x b) (car b))
(define (brick-y b) (cadr b))
(define (brick-v b) (caddr b))
(define (me-x me)(car me))
(define (me-y me)(cadr me))
(define (missile-x mi) (car mi))
(define (missile-y mi) (cadr mi))


;;;; 乱数の定義
(define (randint . args)
  (cond ((= (length args) 1)
         (floor (* (random) (car args))))
        ((= (length args) 2)
         (+ (car args) (floor (* (random) (- (cadr args) (car args))))))
        (else (error 'randint "usage: (randint [lo] hi)"))))

;;;; マップ関数定義
(define (my-map fun . lss)
  (letrec ((iter (lambda (fun lss)
                   (if (null? lss)
                       '()
                       (cons (fun (car lss))
                             (iter fun (cdr lss))))))
           (map-rec (lambda (fun lss)
                      (if (memq '() lss)
                          '()
                          (cons (apply fun (iter car lss))
                                (map-rec fun (iter cdr lss)))))))
    (map-rec fun lss)))



;;;; リスト操作用関数
(define (take ls n)
  (if (or (zero? n) (null? ls))
      '()
      (append (list (car ls)) (take (cdr ls) (- n 1)))))
(define (drop ls n)
  (if (or (zero? n) (null? ls))
      ls
      (drop (cdr ls) (- n 1))))

;;;; n対目のミサイル位置情報を取る
(define (get-missile-2 mi n)  
  (getmissile2-kaiki mi n 0)
  )

(define (getmissile2-kaiki mi n count)
  (cond
    ((null?  mi) '())
    ((zero? (- n 1)) (take mi 2))
    (else (getmissile2-kaiki (cdr (cdr mi)) (- n 1) (+ count 1)))))




;;; シーン設定(レイヤー設定)
;(define (score-scene score) 
;  (place-image (text (number->string score) 20 "white") (+ 30 (/ SCENESIZE-X 2)) (/ SCENESIZE-Y 2)
;               (place-image (text "SCORE :" 20 "white")
;                            (- (/ SCENESIZE-X 2) 30) (/ SCENESIZE-Y 2)
;                            (empty-scene SCENESIZE-X SCENESIZE-Y "black"))))
;
;(define (last-scene) 
;  (place-image (text "END" 20 "white") (/ SCENESIZE-X 2) (/ SCENESIZE-Y 2)
;               (empty-scene SCENESIZE-X SCENESIZE-Y "black")))

;;上記の二つを組み合わせたエンド画面
(define (end-scene score)
  (place-image (text "END" 24 "white") (- (/ SCENESIZE-X 2) 6) (+ (/ SCENESIZE-Y 2) -24)
                                       (place-image (text (number->string score) 20 "white") (+ 30 (/ SCENESIZE-X 2)) (/ SCENESIZE-Y 2)
                                                     (place-image (text "SCORE :" 20 "white")
                                                                  (- (/ SCENESIZE-X 2) 30) (/ SCENESIZE-Y 2)
                                                                  (empty-scene SCENESIZE-X SCENESIZE-Y "black")))))


(define (beginning-scene)
  (place-image (text "READY ?" 30 "white")
               (/ SCENESIZE-X 2)
               (/ SCENESIZE-Y 2)
               (empty-scene SCENESIZE-X SCENESIZE-Y "black"))
  )

(define (scene-main w)
  (place-me (get-me w)
            (place-brick (get-brick w)
                         (place-missile (get-missile w) (get-mi-num w) w (place-time w SCENE)))))

(define (draw-scene w)
  (cond ((< (get-time w) READY-TIME)(beginning-scene))
        ((< (get-time w) FIN-TIME) (scene-main w))      
        (else (end-scene (get-score w)))))


;;;; 描画処理
(define (place-me me s)
  (place-image ME  (me-x me) (me-y me)
               s))
(define (place-brick b s)
  (cond ((or (string? b) (null? b)) s)
        (else (place-image BRICK (brick-x b) (brick-y b) s))))

(define (place-missile mi-whole n w s)
  (cond
    ((string? mi-whole) (place-score (get-score w)   s))
    ((null? mi-whole) (place-score (get-score w)   s))
    (else (missile-saiki mi-whole n w s 0)
     )))

(define (missile-saiki mi-whole n w s count)
  (cond ((zero?  n)          (place-score (get-score w)   s))
        ((null? mi-whole) (place-score (get-score w)   s))
        ((not (missile-alive? (get-missile-2 mi-whole 1) (get-brick w))) (missile-saiki (cdr (cdr mi-whole)) (- n 1) w s (+ 1 count)))
        (else
         (place-image  MISSILE
                       (missile-x (get-missile-2 mi-whole 1))
                       (missile-y (get-missile-2 mi-whole 1))
                       (missile-saiki (cdr (cdr mi-whole)) (- n 1) w s (+ 1 count))))))


(define (place-score score s)
  (place-image (text "SCORE: " 18 "gray") (- SCENESIZE-X 76) 20
               (place-image (text (number->string score) 20  "gray")
                            (- SCENESIZE-X 30)
                            20 
                            s))
  )

(define (place-time w s) (place-image (text "TIME: " 18 "gray") 34 16
                                      (if (< (- (+ FIN-TIME 1) (get-time w)) 10)
                                          (place-image (text (number->string (floor (- (+ FIN-TIME 1) (get-time w)))) 20 "red") 66 16 s)
                                          (place-image (text (number->string (floor (- (+ FIN-TIME 1) (get-time w)))) 18 "gray") 66 16 s))))

  

;;;;;; 世界の状態を更新する処理

;;;;ミサイルと障害物の間の当たり判定条件式
(define (hit? mi-part b)
  (if (null? mi-part) #false
      (if (null? b) #false
          (and (not (string? mi-part));;
               ;(not (null? mi-part))
               (not (string? b));;ミサイルと障害物が生きてるならtrue
               (not (null? b))
               ;(hit-saiki? mi b n 0)
               (< (abs (- (missile-x (get-missile-2 mi-part 1)) (brick-x b)))
                  (/ (+ BRICK-WIDTH MISSILE-SIZE) 2))
               (< (abs (- (missile-y mi-part) (brick-y b)))
                  (/ (+ BRICK-HEIGHT MISSILE-SIZE) 2))
               ))))


(define (hit-saiki? mi-whole b n count w);;  ブール値を返すが、同時に当たった弾の x座標 を null にしといたら便利かもしれない
  (cond ((= n count) #false)
        ((null? mi-whole) #f)
        ((null? b) #f)
        ((string? b) #f)
        ((and ;; このandがtrueの時弾が当たってる      
          (< (abs (- (missile-x mi-whole) (brick-x b)))
             (/ (+ BRICK-WIDTH MISSILE-SIZE) 2))
          (< (abs (- (missile-y mi-whole) (brick-y b)))
             (/ (+ BRICK-HEIGHT MISSILE-SIZE) 2)))
        #true)
        (else (hit-saiki? (cdr (cdr mi-whole)) b n (+ count 1) w))))



;;;;ミサイルの軌道
(define (move-missile mi-part)
  (missile   
     (+ (missile-x mi-part) MISSILE-V)
     (missile-y mi-part))
  )

;;;;障害物の軌道(画面右から左へ突き抜ける)
(define (simple-brick b)
  (brick (+ (brick-x b) (brick-v b))
         (brick-y b)
         (brick-v b)))

;;;;障害物の軌道(壁に反射する)
(define (reflect-brick b)
  (cond ((< (brick-x b) (/ BRICK-WIDTH 2))
         (brick
            (/ BRICK-WIDTH 2)
            (brick-y b)
            (- (brick-v b))))     
        ((> (brick-x b) (- SCENESIZE-X (/ BRICK-WIDTH 2)))
         (brick (- SCENESIZE-X (/ BRICK-WIDTH 2))
                (brick-y b)
                (- (brick-v b))))
        (else (brick (+ (brick-x b) (brick-v b))
                     (brick-y b)          
                     (brick-v b)))))

;;;;ミサイルの消滅条件式
(define (missile-alive? m b)
  (and (not (hit? m b))
       (>= (missile-y m) 0)
       (<= (missile-x m) SCENESIZE-X)))

(define (brick-reach? b)
  (cond
    ((null? b) #true)
    ((< (brick-x b) 0) #true)
    (else #false)
    ))


(define (hit-brick-saiki mi-whole b)
  (if (or (null? mi-whole) (string? mi-whole)) ;;ブロックとミサイル全体を引数にとる。どれか一つでも当たってたらtrueを返す
      #false
      (if (hit? (get-missile-2 mi-whole 1) b)
          #true
          (hit-brick-saiki (cdr (cdr mi-whole)) b)))
  )

;;;;ミサイルの弾道を編集するための再帰
(define (move-saiki mi n count)
  (cond ((null? mi) mi)
        (else (append (my-map +
                              (list MISSILE-V  0 )
                              (get-missile-2 mi 1))
                      (move-saiki (cdr (cdr mi)) n (+ count 1) )))
        ))

(define (missile-next-saiki mi-whole b num)
  (cond ((string? mi-whole) (get-missile-2 mi-whole 1))
        ((= num 0) (get-missile-2 mi-whole 1))
        ((null? (get-missile-2 mi-whole 1)) (append (get-missile-2 mi-whole 1) (missile-next-saiki (cdr (cdr mi-whole)) b (- num 1))))
        ((not (missile-alive? mi-whole b)) (append (list -20 -20) (missile-next-saiki (cdr (cdr mi-whole)) b (- num 1))))
        (else
         (append (my-map +
                         (list MISSILE-V  0)  (get-missile-2 mi-whole 1))
                         (missile-next-saiki (cdr (cdr mi-whole))  b (- num 1))))
         
        ))

;;;;世界の次の瞬間の状態
(define (next w)
  (world (get-me w)
         (cond  
           ((< (get-time w) READY-TIME) (get-brick w))
           ((string? (get-brick w)) (list SCENESIZE-X (randint 15 SCENESIZE-Y) -5))
           ((brick-reach? (get-brick w)) "none")
           ((not (hit-brick-saiki (get-missile w) (get-brick w)))
            (simple-brick (get-brick w)))
           
           (else '()))
         (cond ((string? (get-missile w)) (get-missile w))
               ((not (null? (get-missile w))) (missile-next-saiki (get-missile w) (get-brick w) (get-mi-num w)))
               (else (get-missile w)))
         (+ (get-time w) (/ 1 28))
         (cond ((hit-saiki? (get-missile w) (get-brick w) (get-mi-num w) 0 w)
                (+ (get-score w) 1))
               (else (get-score w))
               )
         (get-mi-num w)
         ))


;;;; キーボード入力処理
(define (control w k)
  (cond ((string=? k "up")
         (world (me (car (get-me w)) (- (cadr (get-me w)) 7))
                (get-brick w)
                (get-missile w)
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k "down")
         (world (me (car (get-me w)) (+ (cadr (get-me w)) 7))
                (get-brick w)
                (get-missile w)               
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k "right")
         (world (me (+ (car (get-me w)) 7) (cadr (get-me w)))
                (get-brick w)
                (get-missile w)           
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k "left")
         (world (me (- (me-x (get-me w)) 7) (me-y (get-me w)))
                (get-brick w)
                (get-missile w)
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k (and "up" "right"))
         (world (me (+ (car (get-me w)) 7) (- (cadr (get-me w)) 7))
                (get-brick w)
                (get-missile w)
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k (and "up" "left"))
         (world (me (- (car (get-me w)) 7) (- (cadr (get-me w)) 7))
                (get-brick w)
                (get-missile w)
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k (and "down" "right"))
         (world (me (+ (car (get-me w)) 7) (+ (cadr (get-me w)) 7))
                (get-brick w)
                (get-missile w)
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        ((string=? k (and "down" "left"))
         (world (me (- (car (get-me w)) 7) (+ (cadr (get-me w)) 7))
                (get-brick w)
                (get-missile w)
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (get-mi-num w)
                ))
        
        ((string=? k " ")
         (world (get-me w)
                (get-brick w)
                (cond ((string? (get-missile w))
                       (missile (me-x (get-me w))
                                (me-y (get-me w))))
                      
                      ;(else (tsuika w (get-missile w)) ;;;;
                      ;(get-missile w)
                      ;(get-missile w))
                      
                      (else (append (get-missile w) (list (me-x (get-me w))
                                                          (me-y (get-me w)))))
                      )
                (+ (get-time w) (/ 1 28))
                (get-score w)
                (+ (get-mi-num w) 1)
                ))
        (else w)))



;;;; アプリケーションの実行を開始
(define (s)
  (big-bang
      (world (me 15 (/ SCENESIZE-Y 2))
             (brick SCENESIZE-X 40 -5)
             ;;;;ミサイルがない場合はnone状態であり、string?にtrueが返ってくる
             "none"
             0
             0
             0)
    
    
    ;;;;画面を呼び出す
    (to-draw draw-scene)
    
    ;;;; on-tickにより28fpsで世界を更新
    (on-tick next)
    
    ;;;; キーボード入力を反映
    (on-key control)
    ))
