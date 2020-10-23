;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter_modify_game_rate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; ==========================================================================================
;; Constants:
;; ==========================================================================================

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

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

(define MISSILE (ellipse 5 15 "solid" "red"))


;;===========================================================================================
;; Data Definitions:
;;===========================================================================================
(define-struct game (invaders missiles tank rate))
;; Game is (make-game  (listof Invader) (listof Missile) Tank Rate)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles, tank position, and invader rate counter

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))
       (fn-for-invader-counter (game-rate s))))



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



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 200 150 -12))

#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; Rate is n
;; interp. the invader rate counter.  Rate has to be smaller than 100.

(define R1 0)
(define R2 50)
(define R3 100)



(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 20))
(define G2 (make-game (list I1) (list M1) T1 50))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 100))

;; ===============================================================================
;; Functions
;; ================================================================================

;; game -> game
;; starts the world with game

(define (main game)
  (big-bang game
    (on-tick game-state) ; game -> game
    (to-draw render)     ; game -> image
    (stop-when terminate) ;game -> Boolean
    (on-key move-fire))) ; game KeyEvent -> game

;; ===============================================================================
;; Next Frame
;; ===============================================================================

;; game -> game
;; produce the next game state

(check-expect (game-state
               (make-game(cons (make-invader 150 100 12) empty)
                         (cons (make-missile 150 300) empty)
                         (make-tank 50 1)
                         50))
              (make-game(cons (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12) empty)
                        (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty)
                        (make-tank 50 1)
                        51))
(check-expect (game-state
               (make-game(cons (make-invader 150 50 -12) empty)
                         (cons (make-missile 150 150) empty)
                         (make-tank 50 1)
                         100))
              (make-game(cons (make-invader (- 150 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -12)
                              (cons (make-invader (random WIDTH) 0 (randomize-number 1)) empty))
                        (cons (make-missile 150 (- 150 MISSILE-SPEED)) empty)
                        (make-tank 50 1)
                        0))

;(define (game-state g) false)
; <template copied from data structure>

(define (game-state g)
  (make-game
   (final-invader-list (game-invaders g) (game-rate g) (game-missiles g))
   (final-missile-list (game-invaders g) (game-rate g) (game-missiles g))
   (game-tank g)
   (invader-rate-counter (game-rate g))))


;; Invaders
;; ============================================================================================

;; ListofInvaders -> ListofInvaders
;; interprets a list of invaders and generates the next list of invaders. Spawns a new invader every time a counter hits the INVADER-RATE
;; !!! Remember to add the random number generator in here
(check-expect (next-invader (cons (make-invader 150 100 12) (cons (make-invader 160 50 -12) (cons (make-invader 200 40 12) empty))))
              (cons (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12)
                    (cons (make-invader (- 160 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -12)
                          (cons (make-invader (+ 200 INVADER-X-SPEED) (+ 40 INVADER-Y-SPEED) 12)
                                empty))))
(check-expect (next-invader (cons (make-invader 0 100 -20) (cons (make-invader WIDTH 250 12) empty)))
              (cons (make-invader (+ 0 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 20)
                    (cons (make-invader (- WIDTH INVADER-X-SPEED) (+ 250 INVADER-Y-SPEED) -12)
                          empty)))


;(define (next-invader loi) empty) ;stub
(define (next-invader loi)
  (cond [(empty? loi) empty]
        [else 
         (cons (eval-invader (first loi))
               (next-invader (rest loi)))]))


;; Invader -> Invader
;; consumes a invader and determines if it should continue on its trajectory or it should change direction
(check-expect (eval-invader (make-invader 0 50 -1))
              (make-invader (+ INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) 1))
(check-expect (eval-invader (make-invader WIDTH 100 1))
              (make-invader (- WIDTH INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -1))
(check-expect (eval-invader (make-invader 50 200 1))
              (make-invader (+ 50 INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) 1))
(check-expect (eval-invader (make-invader 100 50 -1))
              (make-invader (- 100 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) -1))

(define (eval-invader i)
  (if (or (<= (invader-x i) 0) (>= (invader-x i) WIDTH))
      (change-direction i)
      (move-invader i)))


;; Invader -> Invader
;; Consumes a Invader.  If the Invader has reached the left or right edge of the screen, change the direction.
(check-expect (change-direction (make-invader 0 50 -12))
              (make-invader (+ 0 INVADER-X-SPEED) (+ 50 INVADER-Y-SPEED) 12))
(check-expect (change-direction (make-invader WIDTH 70 50))
              (make-invader (- WIDTH INVADER-X-SPEED) (+ 70 INVADER-Y-SPEED) -50))

;(define (change-direction invader) (make-invader 0 0 0)) ;stub

(define (change-direction invader)
  (if (<= (invader-x invader) 0)
      (make-invader (+ 0 INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (* -1 (invader-dx invader)))
      (make-invader (- WIDTH INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (* -1 (invader-dx invader)))))


;; Invader -> Invader
;; consumes a invader and produces the next invader moving normally
(check-expect (move-invader (make-invader 50 60 20))
              (make-invader (+ 50 INVADER-X-SPEED) (+ 60 INVADER-Y-SPEED) 20))
(check-expect (move-invader (make-invader 30 100 -12))
              (make-invader (- 30 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) -12))

;(define (move-invader invader) (make-invader 1 1 1)) ;stub
(define (move-invader invader)
  (if (> (invader-dx invader) 0)
      (make-invader (+ (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-X-SPEED) (invader-dx invader))
      (make-invader (- (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-X-SPEED) (invader-dx invader))))


;; Missile
;; ==========================================================================================
;; ListofMissile -> ListofMissile
;; interperts a list of invadors and genrates the next list of missiles
(check-expect (next-missile empty) empty)
(check-expect (next-missile (cons (make-missile 50 60) empty))
              (cons (make-missile 50 (- 60 MISSILE-SPEED)) empty))
(check-expect (next-missile (cons (make-missile 50 60) (cons (make-missile 70 80) (cons (make-missile 100 200) empty))))
              (cons (make-missile 50 (- 60 MISSILE-SPEED)) (cons (make-missile 70 (- 80 MISSILE-SPEED)) (cons (make-missile 100 (- 200 MISSILE-SPEED)) empty))))
(check-expect (next-missile (cons (make-missile 50 0) (cons (make-missile 40 60) empty)))
              (cons (make-missile 40 (- 60 MISSILE-SPEED)) empty))
(check-expect (next-missile (cons (make-missile 40 -1) (cons (make-missile 100 40) empty)))
              (cons (make-missile 100 (- 40 MISSILE-SPEED)) empty))

; (define (next-missile lom) empty) ;stub

(define (next-missile lom)
  (cond [(empty? lom) empty]
        [else
         (if (out-of-bounds? (first lom))
             (next-missile (rest lom))
             (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
                   (next-missile (rest lom))))]))


;; missile -> Boolean
;; Determine if the missile is out of bounds
(check-expect (out-of-bounds? (make-missile 100 50))
              false)
(check-expect (out-of-bounds? (make-missile 50 0))
              true)

;(define (out-of-bounds? m) false) ;stub
(define (out-of-bounds? m)
  (<= (missile-y m) 0))

;; Random invaders
;; ===============================================================================================================2
;; ListofInvader Number -> ListofInvader
;; Consume a list of invaders and the increment rate.  If the list increment rate reaches 100 then it would create a new random invader at the top of the screen
(check-expect (random-invader (list I1 I2 I3 I4) 50)
              (cons (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                    (cons (make-invader (- (invader-x I2) INVADER-X-SPEED) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                          (cons (make-invader (+ (invader-x I3) INVADER-X-SPEED) (+ (invader-y I3) INVADER-Y-SPEED) (invader-dx I3))
                                (cons (make-invader (- (invader-x I4) INVADER-X-SPEED) (+ (invader-y I4) INVADER-Y-SPEED) (invader-dx I4))
                                      empty)))))
(check-expect (random-invader (list I1 I2 I3 I4) 100)
              (append
               (cons (make-invader (+ (invader-x I1) INVADER-X-SPEED) (+ (invader-y I1) INVADER-Y-SPEED) (invader-dx I1))
                     (cons (make-invader (- (invader-x I2) INVADER-X-SPEED) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                           (cons (make-invader (+ (invader-x I3) INVADER-X-SPEED) (+ (invader-y I3) INVADER-Y-SPEED) (invader-dx I3))
                                 (cons (make-invader (- (invader-x I4) INVADER-X-SPEED) (+ (invader-y I4) INVADER-Y-SPEED) (invader-dx I4))
                                       empty))))
               (cons (make-invader (random WIDTH) 0 1) empty)))

;(define (random-invader loi n) empty) ;stub
(define (random-invader loi n)
  (if (= n INVADE-RATE)
      (append (next-invader loi) (cons (make-invader (random WIDTH) 0 (randomize-number 1)) empty))
      (next-invader loi)))


;; Number -> Number
;; interp. a number and increment it.  Reset it to 0 if it reaches the same time as the INVADER-RATE
(check-expect (invader-rate-counter 0)
              1)
(check-expect (invader-rate-counter INVADE-RATE)
              0)

;(define (invader-rate-counter n) 0) ;stub
(define (invader-rate-counter n)
  (if (= n INVADE-RATE)
      0
      (add1 n)))


;; Number -> Number
;; interp. a number and randomly generates a positive or negative number
(check-expect (randomize-number 50)
              50)
(check-expect (randomize-number 25)
              -25)


;(define (randomize-number n) 0) ;stub
(define (randomize-number n)
  (if (= (modulo (random 50) 2) 1)
      (- n)
      n))


;; Invader List Combination Hit
;; ==============================================================================================
;; ListofInvaders Number ListofMissiles -> ListofInvaders
;; Consumes the result of the next group of invaders and the result of the next list of missiles for final rendering of invaders
(check-expect (final-invader-list (list I1 I2 I3 I4) 100 (list M1 M2))
              (append
               (cons (make-invader (- (invader-x I2) INVADER-X-SPEED) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                     (cons (make-invader (+ (invader-x I3) INVADER-X-SPEED) (+ (invader-y I3) INVADER-Y-SPEED) (invader-dx I3))
                           (cons (make-invader (- (invader-x I4) INVADER-X-SPEED) (+ (invader-y I4) INVADER-Y-SPEED) (invader-dx I4))
                                 empty)))
               (cons (make-invader (random WIDTH) 0 (randomize-number 1)) empty)))
(check-expect (final-invader-list (list I1 I2 I3 I4) 50 (list M1 M2))
              (cons (make-invader (- (invader-x I2) INVADER-X-SPEED) (+ (invader-y I2) INVADER-Y-SPEED) (invader-dx I2))
                    (cons (make-invader (+ (invader-x I3) INVADER-X-SPEED) (+ (invader-y I3) INVADER-Y-SPEED) (invader-dx I3))
                          (cons (make-invader (- (invader-x I4) INVADER-X-SPEED) (+ (invader-y I4) INVADER-Y-SPEED) (invader-dx I4))
                                empty))))

;(define (final-invader-list loi n lom) empty) ;stub
(define (final-invader-list loi n lom)
  (missile-hit-invader (random-invader loi n) (next-missile lom)))


;; ListofInvaders ListofMissiles -> ListofInvaders
;; Consumes a list of invaders and a list of missiles.  Take away any list of invaders where the missile hits.
(check-expect (missile-hit-invader (list I1 I2 I3) (list M1 (make-missile 1 200)))
              (list I1 I2 I3))
(check-expect (missile-hit-invader (list I1 I2 I3) (list M1 M3))
              (list I2 I3))


;(define (missile-hit-invader loi lom) empty) ;stub

(define (missile-hit-invader loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (missile-hit? (first loi) lom)
             (missile-hit-invader (rest loi) lom)
             (cons (first loi)
                   (missile-hit-invader (rest loi) lom)))]))


;; Invader ListofMissiles -> Boolean
;; Consumes a Invader and a list of missiles to determine if the Invader has been hit.
(check-expect (missile-hit? (make-invader 50 50 40) (cons (make-missile 60 60) (cons (make-missile 100 200) empty))) true)
(check-expect (missile-hit? (make-invader 90 190 60) (cons (make-missile 60 60) (cons (make-missile 100 200) empty))) true)
(check-expect (missile-hit? (make-invader 80 80 -10) (cons (make-missile 60 60) (cons (make-missile 100 200) empty))) false)

(define (missile-hit? invader lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (- (invader-x invader) 10) (missile-x (first lom)))
                  (>= (+ (invader-x invader) 10) (missile-x (first lom)))
                  (<= (- (invader-y invader) 10) (missile-y (first lom)))
                  (>= (+ (invader-y invader) 10) (missile-y (first lom))))
             true
             (missile-hit? invader (rest lom)))]))

;; Get Rid of Missiles if they hit
;; ================================================================================
;; ListofInvaders Number ListofMissiles -> ListofMissiles
;; consumes the current list of invaders and the next-missiles to generate the next missiles
(check-expect (final-missile-list
               (cons (make-invader 50 70 -1) (cons (make-invader 80 90 1) empty))
               50
               (cons (make-missile 48 78) (cons (make-missile 100 90) empty)))
              (cons (make-missile 100 (- 90 MISSILE-SPEED)) empty))
(check-expect (final-missile-list (list I2 I3 I4) 60 (list M1 (make-missile 50 10)))
              (list
               (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))
               (make-missile 50 (- 10 MISSILE-SPEED))))

(define (final-missile-list loi n lom)
  (missile-hit-target (random-invader loi n) (next-missile lom)))


;; ListofInvaders ListofMissiles -> ListofMissiles
;; consumes a list of invaders and a list of missiles to determine if missiles have hit the invaders and drop those that have hit off of the list
(check-expect (missile-hit-target (list I1 I2 I3) (list M1 M2))
              (list M1))
(check-expect (missile-hit-target (list I2 I3 I4) (list M1 (make-missile 50 10)))
              (list M1 (make-missile 50 10)))

;(define (missile-hit-target loi lom) empty) ;stub
(define (missile-hit-target loi lom)
  (cond [(empty? lom) empty]
        [else
         (if (hit-target? (first lom) loi)
             (missile-hit-target loi (rest lom))
             (cons (first lom)
                   (missile-hit-target loi (rest lom))))]))


;; Missile ListofInvaders -> Boolean
;; consumes a missile and compares to a ListofInvaders.  Returns true if the missile has hit something.
(check-expect (hit-target? M1 (list I1 I2 I3 I4)) false)
(check-expect (hit-target? M2 (list I1 I2 I3 I4)) true)
(check-expect (hit-target? M3 (list I1 I2 I3 I4)) true)

;(define (hit-target? m loi) false) ;stub
(define (hit-target? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (<= (missile-x m) (+ (invader-x (first loi)) HIT-RANGE))
                  (>= (missile-x m) (- (invader-x (first loi)) HIT-RANGE))
                  (<= (missile-y m) (+ (invader-y (first loi)) HIT-RANGE))
                  (>= (missile-y m) (- (invader-y (first loi)) HIT-RANGE)))
             true
             (hit-target? m (rest loi)))]))

;; ============================================================================================================
;; Rendering
;; ============================================================================================================
;; game -> image
;; renders the current state of the game

(check-expect (render G0)
              (place-image 
               TANK (tank-x T0) (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))
(check-expect (render G1)
              (place-image 
               TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))
(check-expect (render G2)
              (place-image
               INVADER (invader-x I1) (invader-y I1)
               (place-image MISSILE (missile-x M1) (missile-y M1)
                            (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2)
                                         BACKGROUND))))
(check-expect (render G3)
              (place-image
               INVADER (invader-x I1) (invader-y I1)
               (place-image
                INVADER (invader-x I2) (invader-y I2)
                (place-image
                 MISSILE (missile-x M1) (missile-y M1)
                 (place-image
                  MISSILE (missile-x M2) (missile-y M2)
                  (place-image
                   TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2)
                   BACKGROUND))))))

;(define (render g)(square 0 "outline" "white")) ;stub
(define (render g)
  (render-game (game-invaders g) (game-missiles g) (game-tank g) BACKGROUND))


;; ListofInvaders ListofMissiles Tank Background -> Image
;; consumes the lists of invaders, missiles, the tank, and the background to render to.  Produces the game frame.
(check-expect (render-game (list I1 I2) (list M1 M2) T1 BACKGROUND)
              (place-image
               INVADER (invader-x I1) (invader-y I1)
               (place-image
                INVADER (invader-x I2) (invader-y I2)
                (place-image
                 MISSILE (missile-x M1) (missile-y M1)
                 (place-image
                  MISSILE (missile-x M2) (missile-y M2)
                  (place-image
                   TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2)
                   BACKGROUND))))))

;(define (render-game loi lom t bg) BACKGROUND) ;stub

(define (render-game loi lom t bg)
  (cond [(empty? loi)
         (cond [(empty? lom)
                (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) bg)]
               [else
                (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                             (render-game loi (rest lom) t bg))])]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                      (render-game  (rest loi) lom t bg))]))

;; ============================================================================================================
;; Termination
;; ============================================================================================================
;; game -> Boolean
;; stops the game if a invader reaches the bottom edge of the screen
(check-expect (terminate (make-game
                          (cons (make-invader 50 100 -20)
                                (cons (make-invader 100 200 30)
                                      empty))
                          (list M1 M2)
                          T1 50))false)
(check-expect (terminate (make-game (list I1 I2 (make-invader 50 HEIGHT -5)) (list M1 M2) T1 50)) true)

;(define (terminate g) false) ;stub
(define (terminate g)
  (reached-end? (game-invaders g)))


;; ListofInvadersr -> Boolean
;; checks if one of the invaders has reached the edge of the screen
(check-expect (reached-end? (cons (make-invader 50 60 12) (cons (make-invader 100 50 -10) empty)))
              false)
(check-expect (reached-end? (list I1 I2 I3 (make-invader 50 HEIGHT -12)))
              true)

;(define (reached-end? loi) false) ;stub
(define (reached-end? loi)
  (cond [(empty? loi) false]
        [else
         (if (<= HEIGHT (invader-y (first loi)))
             true
             (reached-end? (rest loi)))]))


;; ==============================================================================================================
;; Key Events
;; ==============================================================================================================

;; KeyEvent -> game
;; moves the tank left or right depending on the KeyEvent
(check-expect (move-fire G3 "left")
              (make-game (list I1 I2) (list M1 M2) (make-tank (- (tank-x (game-tank G3)) TANK-SPEED) -1) 100))
(check-expect (move-fire (make-game (list I1 I2) (list M1 M2) (make-tank 0 -1) 100) "left")
              (make-game (list I1 I2) (list M1 M2) (make-tank 0 -1) 100))
(check-expect (move-fire G3 "right")
              (make-game (list I1 I2) (list M1 M2) (make-tank (+ (tank-x (game-tank G3)) TANK-SPEED)  1) 100))
(check-expect (move-fire (make-game (list I1 I2) (list M1 M2) (make-tank WIDTH 1) 100) "right")
              (make-game (list I1 I2) (list M1 M2) (make-tank WIDTH 1) 100))                         
(check-expect (move-fire G3 " ")
              (make-game (list I1 I2) (list M1 M2 (make-missile (tank-x (game-tank G3)) (- HEIGHT TANK-HEIGHT/2))) T1 100))
(check-expect (move-fire G3 "up")
              (make-game (list I1 I2) (list M1 M2) T1 100))

;(define (move-fire game ke) false) ;stub

(define (move-fire g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g)
                    (append (game-missiles g)
                            (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) empty))
                    (game-tank g)
                    (game-rate g))]
        [(key=? ke "left")
         (if (> (tank-x (game-tank g)) 0)
             (make-game (game-invaders g) (game-missiles g) (make-tank (- (tank-x (game-tank g)) TANK-SPEED) -1)(game-rate g))
             (make-game (game-invaders g) (game-missiles g) (game-tank g) (game-rate g)))]
        [(key=? ke "right")
         (if (< (tank-x (game-tank g)) WIDTH)
             (make-game (game-invaders g) (game-missiles g) (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) 1)(game-rate g))
             (make-game (game-invaders g) (game-missiles g) (game-tank g)(game-rate g)))]
        [else 
         (make-game (game-invaders g) (game-missiles g) (game-tank g)(game-rate g))]))