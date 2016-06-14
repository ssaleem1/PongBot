#lang class/1

(require 2htdp/image)
(require class/universe)

;a Ball is a (new ball% Number Number Number Number) and implements IPong

;a Paddle is a (new paddle% Number Side) and implements IPong
;a Side is one of:
; -'left
; -'right

;a IPong can do:
; -y
; -render
; -render-on
; -next

;a World is a (new world% Ball Paddle Paddle Number Number)

(define WIDTH 500)
(define HEIGHT 500)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define PAD-DELTA 5)
(define BALL-RADIUS 10)
(define PAD-WIDTH 5)
(define PAD-HEIGHT 50)
(define COMP-SPEED 2)

(define-class ball%
  (fields x y vx vy)
  
  ;render: -> Image
  ;renders the ball
  (check-expect (ball1 . render) (circle 10 'solid 'red))
  
  (define (render)
    (circle BALL-RADIUS 'solid 'red))
  
  ;render-on: Image -> Image
  ;renders the ball on the given image
  (check-expect (ball1 . render-on BACKGROUND)
                (place-image (ball1 . render) 67 345 BACKGROUND))
  
  (define (render-on img)
    (place-image 
     (this . render)
     (this . x)
     (this . y)
     img))
  
  ;next: -> Ball
  ;ticks this ball
  (check-expect (ball1 . next)
                (new ball% 90 339 23 -6))
  
  (define (next)
    (new ball% 
         (+ (this . x) (this . vx))
         (+ (this . y) (this . vy))
         (this . vx)
         (this . vy))))



(define-class paddle%
  (fields y side)
  
  ;render: -> Image
  ;renders the paddle
  (check-expect (pad1 . render) (rectangle 5 50 'solid 'black))
  
  (define (render)
    (rectangle PAD-WIDTH PAD-HEIGHT 'solid 'black))
  
  ;render-on: Image -> Image
  ;renders the ball on the given image
  (check-expect (pad1 . render-on BACKGROUND)
                (place-image (pad1 . render) 2.5 241 BACKGROUND))
  (check-expect (pad2 . render-on BACKGROUND)
                (place-image (pad2 . render) 497.5 50 BACKGROUND))
  
  (define (render-on img)
    (place-image
     (this . render)
     (if (symbol=? (this . side) 'left)
         (/ (image-width (this . render)) 2)
         (- WIDTH (/ (image-width (this . render)) 2)))
     (this . y)
     img))
  
  ;collide?: Ball -> Boolean
  ;has this paddle collided with the given ball?
  (check-expect (pad1 . collide? collide1) true)
  (check-expect (pad2 . collide? collide2) true)
  (check-expect (pad1 . collide? ball1) false)
  (check-expect (pad2 . collide? ball1) false)
  
  (define (collide? b)
    (if (symbol=? (this . side) 'left)
        (and
         (<= (b . x) PAD-WIDTH)
         (and (< (b . y) (+ (this . y) (/ PAD-HEIGHT 2)))
              (> (b . y) (- (this . y) (/ PAD-HEIGHT 2)))))
        (and
         (>= (b . x) (- WIDTH PAD-WIDTH))
         (and (< (b . y) (+ (this . y) (/ PAD-HEIGHT 2)))
              (> (b . y) (- (this . y) (/ PAD-HEIGHT 2)))))))
  
  ;move-to: Ball -> Paddle
  ;moves this paddle closer to the ball's height
  (define (move-to b)
    (cond
      [(<= (b . y)  (this . y))
       (new paddle% (- (this . y) COMP-SPEED) (this . side))]
      [else (new paddle% (+ (this . y) COMP-SPEED) (this . side))])))

(define-class world%
  (fields ball pad1 pad2 score1 score2)
  
  ;to-draw: -> Image
  ;draws this world
  (check-expect (world1 . to-draw)
                (place-image
                 (beside (text "4" 30 "black")
                         (text "   " 30 "black")
                         (text "5" 30 "black"))
                 (/ WIDTH 2)
                 50
                 (ball1 . render-on
                        (pad1 . render-on
                              (pad2 . render-on BACKGROUND)))))
  (define (to-draw)
    (place-image
     (beside
      (text (number->string (this . score1)) 30 "black")
      (text "   " 30 "black")
      (text (number->string (this . score2)) 30 "black"))
     (/ WIDTH 2)
     50
     ((this . ball) . render-on 
                    ((this . pad1) . render-on
                                   ((this . pad2) . render-on BACKGROUND)))))
  
  ;on-tick: -> World
  ;ticks the world
  (check-expect (world1 . on-tick)
                (new world% (new ball% 90 339 23 -6)
                     (new paddle% 241 'left) (new paddle% 50 'right) 4 5))
  
  
  (define (on-tick)
    ((((this . move) . bounce) . score-check) . move-comp-pad))
  
  ;move-comp-pad: -> World
  ;moves the computer paddle
  (define (move-comp-pad)
    (new world%
         (this . ball)
         (this . pad1)
         ((this . pad2) . move-to (this . ball))
         (this . score1)
         (this . score2)))
  
  ;score-check: -> World
  ;checks if anyone has scored and updates the counter if they have
  (check-expect (lworld . score-check)
                (new world% (new ball% 250 250 5 2)
                     (new paddle% 241 'left) (new paddle% 50 'right) 0 1))
  (check-expect (rworld . score-check)
                (new world% (new ball% 250 250 5 2)
                     (new paddle% 241 'left) (new paddle% 50 'right) 1 0))
  (check-expect (world1 . score-check) world1)
  
  (define (score-check)
    (cond 
      [(and
        (< ((this . ball) . x) PAD-WIDTH)
        (not ((this . pad1) . collide? (this . ball))))
       (this . right-score)]
      [(and
        (> ((this . ball) . x) (- WIDTH PAD-WIDTH))
        (not ((this . pad2) . collide? (this . ball))))
       (this . left-score)]
      [else
       this]))
  
  ;right-score: -> World
  ;adds 1 to the right pad's score
  (check-expect (lworld . right-score)
                (new world% (new ball% 250 250 5 2)
                     (new paddle% 241 'left) (new paddle% 50 'right) 0 1)) 
  
  (define (right-score)
    (new world%
         (new ball% (/ WIDTH 2) (/ HEIGHT 2) 5 2)
         (this . pad1)
         (this . pad2)
         (this . score1)
         (add1 (this . score2))))
  
  ;left-score: -> World
  ;adds 1 to the right pad's score
  (check-expect (rworld . left-score)
                (new world% (new ball% 250 250 5 2)
                     (new paddle% 241 'left) (new paddle% 50 'right) 1 0))
  
  (define (left-score)
    (new world%
         (new ball% (/ WIDTH 2) (/ HEIGHT 2) 5 2)
         (this . pad1)
         (this . pad2)
         (add1 (this . score1))
         (this . score2)))
  
  
  ;move: -> World
  ;moves the ball
  (check-expect (world1 . move)
                (new world% (new ball% 90 339 23 -6)
                     (new paddle% 241 'left) 
                     (new paddle% 50 'right)
                     4
                     5))
  
  (define (move)
    (new world% 
         ((this . ball) . next)
         (this . pad1)
         (this . pad2)
         (this . score1)
         (this . score2)))
  
  ;bounce: -> World
  ;checks if the ball hit one of the walls or pads and bounces it
  (check-expect (ceiling-world . bounce)
                (new world% (new ball% 67 0 45 6) 
                     (new paddle% 241 'left) (new paddle% 50 'right) 3 2))
  (check-expect (floor-world . bounce)
                (new world% (new ball% 67 500 45 -6) 
                     (new paddle% 241 'left) (new paddle% 50 'right) 2 4))
  (check-expect (pad1-world . bounce) 
                (new world% (new ball% 4 238 -56 -78) 
                     (new paddle% 241 'left) (new paddle% 50 'right) 5 3))
  (check-expect (pad2-world . bounce)
                (new world% (new ball% 497 54 -8 -7) 
                     (new paddle% 241 'left) (new paddle% 50 'right) 5 6))
  
  
  
  (define (bounce)
    (cond
      [(or
        (and (> ((this . ball) . vy) 0) 
             (>= (+ ((this . ball) . y) BALL-RADIUS) HEIGHT))
        ;ball has hit floor
        (and (< ((this . ball) . vy) 0) 
             (<= (- ((this . ball) . y) BALL-RADIUS) 0)))
       ;ball has hit ceiling
       (new world%
            (new ball% 
                 ((this . ball) . x)
                 ((this . ball) . y)
                 ((this . ball) . vx)
                 (* -1 ((this . ball) . vy)))
            (this . pad1)
            (this . pad2)
            (this . score1)
            (this . score2))]
      [(or
        ((this . pad2) . collide? (this . ball))
        ((this . pad1) . collide? (this . ball)))
       (new world%
            (new ball% 
                 ((this . ball) . x)
                 ((this . ball) . y)
                 (* -1 ((this . ball) . vx))
                 ((this . ball) . vy))
            (this . pad1)
            (this . pad2)
            (this . score1)
            (this . score2))]
      [else
       this]
      ))
  
  
  ;on-key: KeyEvent -> World
  ;updates the world depending on which key was pressed
  (check-expect (world1 . on-key "w")
                (new world% 
                     (new ball% 67 345 23 -6)
                     (new paddle% 236 'left) 
                     (new paddle% 50 'right) 4 5))
  (check-expect (world1 . on-key "s")
                (new world% 
                     (new ball% 67 345 23 -6)
                     (new paddle% 246 'left) 
                     (new paddle% 50 'right) 4 5))
  
  (define (on-key ke)
    (cond
      [(string=? ke "w") (new world%
                              (this . ball)
                              (new paddle% (- ((this . pad1) . y) PAD-DELTA) 
                                   ((this . pad1) . side))
                              (this . pad2)
                              (this . score1)
                              (this . score2))]
      [(string=? ke "s") (new world%
                              (this . ball)
                              (new paddle% (+ ((this . pad1) . y) PAD-DELTA) 
                                   ((this . pad1) . side))
                              (this . pad2)
                              (this . score1)
                              (this . score2))]
      [else this]))
  
  )

;init-world: -> World
;creates an initial world
(define (init-world)
  (new world%
       (new ball% (/ WIDTH 2) (/ HEIGHT 2) 5 2)
       (new paddle% (/ HEIGHT 2) 'left)
       (new paddle% (/ HEIGHT 2) 'right)
       0
       0))

(define (main)
  (big-bang (init-world)))

;examples
(define ball1 (new ball% 67 345 23 -6))
(define collide1 (new ball% 4 238 56 -78))
(define collide2 (new ball% (- WIDTH 3) 54 8 -7))
(define ceiling-ball (new ball% 67 0 45 -6))
(define floor-ball (new ball% 67 HEIGHT 45 6))

(define pad1 (new paddle% 241 'left))
(define pad2 (new paddle% 50 'right))

(define world1 (new world% ball1 pad1 pad2 4 5))
(define floor-world (new world% floor-ball pad1 pad2 2 4))
(define ceiling-world (new world% ceiling-ball pad1 pad2 3 2))
(define pad1-world (new world% collide1 pad1 pad2 5 3))
(define pad2-world (new world% collide2 pad1 pad2 5 6))

(define lworld (new world% (new ball% 0 5 23 -6)
                    pad1
                    pad2
                    0
                    0))
(define rworld (new world% (new ball% WIDTH 5 23 -6)
                    pad1
                    pad2
                    0
                    0))



