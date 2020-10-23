;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Test-Space) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Get Rid of Missiles if they hit
;; ================================================================================
;; ListofInvaders Number ListofMissiles -> ListofMissiles
;; consumes the current list of invaders and the next-missiles to generate the next missiles
(check-expect (missile-hit-target (list I1 I2 I3) (list M1 M2))
              (list M1))
(check-expect (missile-hit-target (list I2 I3 I4) (list M1 (make-missile 50 10)))
              (list M1 (make-missile 50 10)))

(define (final-missile-list loi n lom)
  (missile-hit-target (missile-hit-invader (random-invader loi n)) lom))


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