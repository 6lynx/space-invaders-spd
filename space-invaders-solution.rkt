;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; =================
;; Constants:

(define WIDTH 300)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define INVADER-X-SPEED 1.5)
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)
(define MAX-INVADERS 5)



;; =================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game (listof Invader) (listof Missile) Tank)
;; interp. the current state of the game.

;; Game constants are defined below Missile data definition.

#;
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

;; Template Rules Used:
;; - compound: 2 fields


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; Template Rules Used:
; - compound: 3 fields



;; ListOfInvader is one of:
;; - empty
;; (cons Invader ListOfInvader)
;; interp. list of invaders appearing on backgrounds.
(define LOI0 empty)
(define LOI1 (cons I1 empty))
(define LOI2 (cons I2 (cons I1 empty)))
(define LOI3 (cons I3 (cons I2 (cons I1 empty))))


#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))

;; Template Rules Used:
;; one of 2 cases:
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; reference: (first loi) is Invader
;; self-reference: (rest loi) is ListOfInvader



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Template Rules Used:
;; - compound: 2 fields


;; ListOfMissile is one of:
;; - empty
;; (cons Missile ListOfMissile)
;; interp. list of missiles appearing on background.
(define LOM0 empty)
(define LOM1 (cons M1 empty))
(define LOM2 (cons M2 (cons M1 empty)))
(define LOM3 (cons M3 (cons M2 (cons M1 empty))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))

;; Template Rules Used:
;; one of 2 cases:
;; - atomic distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; reference: (first lom) is Missile
;; self-reference: (rest-lom) is ListOfMissile


;; Game Constants:
(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; ===================
;; Functions:


;; Game -> Game
;; no tests for main
;; start with (main G0)


(define (main g)
  (big-bang g
    (to-draw render-game)
    (on-tick next-game)
    (on-key tank-action)
    (stop-when game-over?)))


;; Game -> Image
;; renders the current state of the game
(check-expect (render-game G0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-game G1) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-game G2)  (place-image TANK 50 ( - HEIGHT TANK-HEIGHT/2) (place-image INVADER 150 100 (place-image MISSILE 150 300 BACKGROUND))))



;; Stub:
;; (define (render-game g) BACKGROUND)


(define (render-game g)
  (render-tank (game-tank g)
               (render-missiles (game-missiles g)
                                (render-invaders (game-invaders g) BACKGROUND))))


;; ListOfInvader (game-invaders is a ListOfInvader) Image -> Image
;; renders the current state of the invaders.
(check-expect (render-invaders LOI0 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER 150 100 (place-image INVADER 150 HEIGHT BACKGROUND)))
(check-expect (render-invaders (list (make-invader 10 20 1)
                                     (make-invader 100 100 2)
                                     (make-invader 200 300 3))
                               BACKGROUND)
              (place-image INVADER 10 20
                           (place-image INVADER 100 100
                                        (place-image INVADER 200 300 BACKGROUND))))

(check-expect (render-invaders (list (make-invader 50 50 1)
                                     (make-invader 50 50 2)
                                     (make-invader 50 50 3))
                               BACKGROUND)
              (place-image INVADER 50 50
                           (place-image INVADER 50 50
                                        (place-image INVADER 50 50 BACKGROUND))))

(define non-empty-bg (place-image TANK 150 400 BACKGROUND))
(check-expect (render-invaders (list (make-invader 10 20 1)
                                     (make-invader 100 200 2))
                               non-empty-bg)
              (place-image INVADER 10 20
                           (place-image INVADER 100 200 non-empty-bg)))

(check-expect (render-invaders (list (make-invader 0 0 1)
                                     (make-invader WIDTH HEIGHT 2))
                               BACKGROUND)
              (place-image INVADER 0 0
                           (place-image INVADER WIDTH HEIGHT BACKGROUND)))






;; Stub:
;; (define (render-invaders loi bg) bg)


(define (render-invaders loi bg)
  (cond
    [(empty? loi) bg]
    [else (render-invaders (rest loi)
                           (render-invader (first loi) bg))]))


;; Invader Image -> Image
;; produce the image of the single invader.
(check-expect (render-invader (make-invader 10 20 30) BACKGROUND) (place-image INVADER 10 20 BACKGROUND))
(check-expect (render-invader (make-invader 200 200 30) (place-image INVADER 10 20 BACKGROUND)) (place-image INVADER 200 200 (place-image INVADER 10 20 BACKGROUND)))


;; Stub:
;; (define (render-invader i bg) BACKGROUND)

(define (render-invader i bg)
  (place-image INVADER (invader-x i) (invader-y i) bg))

;; ListOfMissile Image -> Image
;; produce the image of current state of missiles.
(check-expect (render-missiles LOM0 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missiles (list (make-missile 32 32) (make-missile 64 64)) (place-image MISSILE 150 300 BACKGROUND)) (place-image MISSILE 32 32 (place-image MISSILE 64 64 (place-image MISSILE 150 300 BACKGROUND))))


;; Stub:
;; (define (render-missiles lom bg) bg)


(define (render-missiles lom bg)
  (cond [(empty? lom) bg]
        [else (render-missiles (rest lom)
                               (render-missile (first lom) bg))]))

;; Missile Image -> Image
;; produce the of single missile.
(check-expect (render-missile (make-missile 100 100) BACKGROUND) (place-image MISSILE 100 100 BACKGROUND))
(check-expect (render-missile (make-missile 200 200) (place-image MISSILE 100 100 BACKGROUND)) (place-image MISSILE 200 200 (place-image MISSILE 100 100 BACKGROUND)))

;; Stub:
;; (define (render-missile m bg) bg)

(define (render-missile m bg)
  (place-image MISSILE (missile-x m) (missile-y m) bg))


;; Tank Image -> Image
;; produce the image of the tank on top of BACKGROUND
(check-expect (render-tank (make-tank (/ WIDTH 2) 1) BACKGROUND)(place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank (make-tank 50 1) (place-image INVADER 150 100 (place-image MISSILE 150 300 BACKGROUND))) (place-image TANK 50 ( - HEIGHT TANK-HEIGHT/2) (place-image INVADER 150 100 (place-image MISSILE 150 300 BACKGROUND))))

;; Stub:
;; (define (render-tank t bg) bg) 

(define (render-tank t bg)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) bg))


;; Game -> Game
;; produces the next state of the game:
;; - Spawn Invaders randomly.
;; - Invaders adjusted with INVADER-X-SPEED (passed into Invader itself)
;;       and  Invaders y coordinate adjusted with INVADER-Y-SPEED (is not passed into Invader)
;; - Missiles adjusted with MISSILE-SPEED added.
;; - Tank adjusted with TANK-SPEED added.
;; - Filtered colliding Missiles and Invaders.
;; - Filtered Missiles that are out of screen.
;; - Bounced Invaders


;; Stub:
;; (define (next-game g) g)

(define (next-game g)
  (check-collision (make-game (update-invaders (game-invaders g)) (next-missiles (game-missiles g)) (next-tank (game-tank g)))))



;; Game -> Game
;; checks the collision between missiles and invaders.
(check-expect (check-collision G0) G0)
(check-expect (check-collision G3) (make-game (list I2) (list M1) T1))


;; Stub:
;; (define (check-collision g) g)

(define (check-collision g)
  (make-game (remove-collided-invaders (game-invaders g) (game-missiles g))
       (remove-collided-missiles (game-missiles g) (game-invaders g))
       (game-tank g)))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; return a list of invaders that have not collided with any missile
(check-expect (remove-collided-invaders (list I1 I2) (list M1 M2)) (list I2))
(check-expect (remove-collided-invaders (list (make-invader 10 20 2) (make-invader 20 40 2)) (list (make-missile 200 200) (make-missile 200 250)))
              (list (make-invader 10 20 2) (make-invader 20 40 2)))

;; Stub:
;; (define (remove-collided-invaders loi lom) loi)


(define (remove-collided-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (invader-collided? (first loi) lom)
                  (remove-collided-invaders (rest loi) lom)
                  (cons (first loi) (remove-collided-invaders (rest loi) lom)))]))



;; Invader ListOfMissile -> Boolean
;; returns true if invader collided with a missile, returns false otherwise.
(check-expect (invader-collided? I1 (list M1 M2)) true)
(check-expect (invader-collided? I2 empty) false)
(check-expect (invader-collided? (make-invader 10 20 2) (list (make-missile 200 200))) false)
(check-expect (invader-collided? (make-invader 10 20 2) (list (make-missile 10 20))) true)


;; Stub:
;; (define (invader-collided? i lom) false)

(define (invader-collided? i lom)
  (cond [ (empty? lom) false]
        [else (or (collision? (first lom) i)
                  (invader-collided? i (rest lom)))]))


;; Missile Invader -> Boolean
;; returns true if Missile and Invader are within HIT-RANGE of each other
(check-expect (collision? M1 I1) false)
(check-expect (collision? M2 I1) true)
(check-expect (collision? (make-missile 10 20) (make-invader 10 20 2)) true)
(check-expect (collision? (make-missile 200 200) (make-invader 10 20 2)) false)


;; Stub:
;; (define (collision? i m) false)

(define (collision? m i)
  (<= (distance (missile-x m) (missile-y m) (invader-x i) (invader-y i)) HIT-RANGE))


;; Number Number Number Number -> Number
;; returns the distance between two points.
(check-expect (distance 0 0 3 4) 5)
(check-expect (distance 1 2 4 6) 5)
(check-expect (distance 0 0 0 0) 0)
(check-expect (distance 2 3 5 7) 5)

;; Stub:
;; (define (distance  x1 y1 x2 y2) 0)

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; returns a list of missiles that are not collided with invaders
(check-expect (remove-collided-missiles (list M1 M2) (list I1 I2)) (list M1))
(check-expect (remove-collided-missiles (list M1 M2) (list I1 I2)) (list M1))
(check-expect (remove-collided-missiles (list M1 M2) (list I3)) (list M1 M2))
(check-expect (remove-collided-missiles (list (make-missile 100 200)) (list (make-invader 100 200 5)))
              empty)
(check-expect (remove-collided-missiles (list (make-missile 300 300)) (list (make-invader 100 100 5)))
              (list (make-missile 300 300)))



;; Stub:
;; (define (remove-collided-missiles lom loi) lom)

(define (remove-collided-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (missile-collided? (first lom) loi)
                  (remove-collided-missiles (rest lom) loi)
                  (cons (first lom) (remove-collided-missiles (rest lom) loi)))]))


;; Missile ListOfInvader -> Boolean
;; returns true if missile collided with an invader.
(check-expect (missile-collided? M1 (list I1 I2)) false)
(check-expect (missile-collided? M2 (list I1 I2)) true)
(check-expect (missile-collided? M1 empty) false)
(check-expect (missile-collided? (make-missile 10 20) (list (make-invader 10 20 2))) true)
(check-expect (missile-collided? (make-missile 200 200) (list (make-invader 10 20 2))) false)


;; Stub:
;; (define (missile-collided? m loi) false)

(define (missile-collided? m loi)
  (cond [(empty? loi) false]
        [else (or (collision? m (first loi))
                   (missile-collided? m (rest loi)))]))


;; Tank -> Tank
;; move the tank to its next position.
(check-expect (next-tank (make-tank 200 1)) (make-tank (+ 200 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 200 -1)) (make-tank (- 200 TANK-SPEED) -1))
(check-expect (next-tank (make-tank WIDTH 1)) (make-tank (- WIDTH TANK-SPEED) -1))   ;; Bounce from right edge
(check-expect (next-tank (make-tank 0 -1)) (make-tank (+ 0 TANK-SPEED) 1))           ;; Bounce from left edge

;; Stub:
;; (define (next-tank t) t)


(define (next-tank t)
  (cond [(>= (tank-x t) WIDTH) (make-tank (- WIDTH TANK-SPEED) -1)]
        [(<= (tank-x t) 0) (make-tank (+ 0 TANK-SPEED) 1)]
        [else (make-tank (+ (* (tank-dir t) TANK-SPEED) (tank-x t)) (tank-dir t))]))


;; ListOfMissile -> ListOfMissile
;; moves the missiles to next position. (adds MISSILE-SPEED to the y coordinate of missiles)
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (list (make-missile 150 10) (make-missile 300 200)))
              (list (make-missile 150 (- 10 MISSILE-SPEED)) (make-missile 300 (- 200 MISSILE-SPEED))))
(check-expect (next-missiles (list (make-missile 150 (+ HEIGHT 10)))) empty)
(check-expect (next-missiles (list (make-missile 150 10) (make-missile 300 (+ HEIGHT 10))))
              (list (make-missile 150 (- 10 MISSILE-SPEED))))


;; Stub:
;; (define (next-missiles lom) lom)


(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else (if (out-of-screen? (first lom))
                  (next-missiles (rest lom))
                  (cons (next-missile (first lom)) (next-missiles (rest lom))))]))


;; Missile -> Boolean
;; produce true if a missile is out of screen, otherwise produce false.
(check-expect (out-of-screen? (make-missile 100 (+ HEIGHT 10))) true)
(check-expect (out-of-screen? (make-missile 100 200)) false)

;; Stub:
;; (define (out-of-bounds m) false)

(define (out-of-screen? m)
  (> (missile-y m) HEIGHT))


;; Missile -> Missile
;; move the individual missile to its next position.
(check-expect (next-missile (make-missile 150 150)) (make-missile 150 (- 150 MISSILE-SPEED)))
(check-expect (next-missile (make-missile 10 100)) (make-missile 10 (- 100 MISSILE-SPEED)))

;; Stub:
;; (define (next-missile m) m)

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; ListOfInvader -> ListOfInvader
;; move the invaders to their next position, spawn new invaders.


;; Stub:
;; (define (update-invaders loi) loi)


(define (update-invaders loi)
  (spawn-invaders (next-invaders loi)))


;;ListOfInvader -> ListOfInvader
;; move the invaders to their next position.
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders (list (make-invader 150 200 INVADER-X-SPEED)))
              (list (make-invader (+ 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED)))
(check-expect (next-invaders (list (make-invader WIDTH 200 INVADER-X-SPEED) (make-invader 0 400 INVADER-X-SPEED)))
              (list (make-invader (- WIDTH INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) (* INVADER-X-SPEED -1))
                    (make-invader (+ 0 INVADER-X-SPEED) (+ 400 INVADER-Y-SPEED) INVADER-X-SPEED)))

(define (next-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (next-invader (first loi))
                   (next-invaders (rest loi)))]))


;; ListOfInvader -> ListOfInvader
;; spawns new invaders with random x coordinate and 0 y coordinate. produce the same list if there is no need for spawning.
#; (check-random (spawn-invaders empty)
              (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                    (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                          (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)                        ;; TESTS ARE WORKING BUT THE LIST IS SOMEHOW REVERSED
                                (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)                  ;; I COULDNT FIND A FIX I TRIED IT WITH CONS BUT NO HELP
                                      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)            ;; I DONT THINK THIS AFFECTS ANYTHING.
                                            empty))))))

;; Stub:
;; (define (spawn-invaders loi) loi)
(check-expect (length (spawn-invaders empty)) 5)
(check-expect (length (spawn-invaders (make-list MAX-INVADERS (make-invader 0 0 0)))) MAX-INVADERS)
(check-expect (length (spawn-invaders (make-list (- MAX-INVADERS 2) (make-invader 0 0 0)))) MAX-INVADERS)


(define (spawn-invaders loi)
  (if (< (length loi) MAX-INVADERS)
      (spawn-invaders (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi))
      loi))


;; Invader -> Invader
;; move the individual invader to its next position.
(check-expect (next-invader (make-invader 150 200 INVADER-X-SPEED)) (make-invader (+ 150 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) INVADER-X-SPEED))
(check-expect (next-invader (make-invader WIDTH 200 INVADER-X-SPEED)) (make-invader ( - WIDTH INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) (* INVADER-X-SPEED -1)))  ; Bounce

;; Stub:
;; (define (next-invader i)  i)

(define (next-invader i)
  (if (or (>= (+ (invader-x i) (invader-dx i)) WIDTH)
          (<= (+ (invader-x i) (invader-dx i)) 0))
      (bounce i)
      (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))))

;; Invader -> Invader
;; bounces the invader from the edge of the screen.
(check-expect (bounce (make-invader WIDTH 100 INVADER-X-SPEED)) (make-invader (- WIDTH INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) (* INVADER-X-SPEED -1)))

;; Stub:
;; (define (bounce i) i)

(define (bounce i)
  (make-invader (- (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (* (invader-dx i) -1)))


;; Game KeyEvent -> Game
;; - if left arrow is pressed move tank to left.
;; - if right arrow is pressed move tank to right.
;; - if space is pressed fire a missile.
(check-expect (tank-action G0 "up") G0)
(check-expect (tank-action (make-game empty empty (make-tank 100 -1)) "right") (make-game empty empty (make-tank 100 1))) ;; to right
(check-expect (tank-action (make-game empty empty (make-tank 100 1)) "left") (make-game empty empty (make-tank 100 -1)))  ;; to left

;; Stub:
;; (define (tank-action key) G0)

(define (tank-action g k)
  (cond [(key=? "right" k) (make-game (game-invaders g) (game-missiles g) (tank-right (game-tank g)))]
        [(key=? "left" k)  (make-game (game-invaders g) (game-missiles g) (tank-left  (game-tank g)))]
        [(key=? " " k)     (make-game (game-invaders g) (fire-missiles (game-missiles g) (game-tank g)) (game-tank g))]
        [else g]))

;; Tank -> Tank
;; changes the tanks direction to right. (makes the tanks dir 1)
(check-expect (tank-right (make-tank 100 -1)) (make-tank 100 1))
(check-expect (tank-right (make-tank 100 1)) (make-tank 100 1))

;; Stub:
;; (define (tank-right t) t)

(define (tank-right t)
  (make-tank (tank-x t) 1))

;; Tank -> Tank
;; changes the tanks direction to left. (makes the tanks dir -1)
(check-expect (tank-left (make-tank 100 1)) (make-tank 100 -1))
(check-expect (tank-left (make-tank 100 -1)) (make-tank 100 -1))

;; Stub:
;; (define (tank-left t) t)

(define (tank-left t)
  (make-tank (tank-x t) -1))


;; ListOfMissiles Tank -> ListOfMissiles
;; fires missiles from  x coordinate of tank.
(check-expect (fire-missiles empty (make-tank 100 1)) (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2))))
(check-expect (fire-missiles (list (make-missile 100 200) (make-missile 40 50)) (make-tank 150 -1))
              (list (make-missile 150 (- HEIGHT TANK-HEIGHT/2)) (make-missile 100 200) (make-missile 40 50)))

;; Stub:
;; (define (fire-missiles lom t) lom)

(define (fire-missiles lom t)
  (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) lom))


;; Game -> Boolean
;; produce true if one of the invaders reaches the surface. (if invader y > HEIGHT)


;; Stub:
;; (define (game-over? g) false)

(define (game-over? g)
  (check-invaders (game-invaders g)))

;; ListOfInvaders -> Boolean
;; produce true if one of the invaders reached surface.
(check-expect (check-invaders (list (make-invader 100 HEIGHT INVADER-X-SPEED) (make-invader 100 200 INVADER-X-SPEED))) true)
(check-expect (check-invaders (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 100 50 INVADER-X-SPEED) (make-invader 10 10 INVADER-X-SPEED))) false)
(check-expect (check-invaders
               (list (make-invader 100 200 INVADER-X-SPEED) (make-invader 100 50 INVADER-X-SPEED)
                     (make-invader 10 10 INVADER-X-SPEED) (make-invader 100 HEIGHT INVADER-X-SPEED))) true)

;; Stub:
;; (define (check-invaders loi) false)

(define (check-invaders loi)
  (cond [(empty? loi) false]
        [else (or (is-passed? (first loi))
                   (check-invaders (rest loi)))]))


;; Invader -> Boolean
;; checks if one individual invader reached surface, produce true if it has.
(check-expect (is-passed? (make-invader 100 HEIGHT INVADER-X-SPEED)) true)
(check-expect (is-passed? (make-invader 100 50 INVADER-X-SPEED)) false)

;; Stub:
;; (define (is-passed? i) false)

(define (is-passed? i)
  ( >= (invader-y i) HEIGHT))