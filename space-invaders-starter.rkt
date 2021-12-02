;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; TODO Problems:
;; I wanted to add missiler at every X ticks, but I realised I can't use 2 on-tick in the same bang
;; so I tried to build it into the one with a ticker, but I couldn't figure out how to put in another variable without
;; much work so I just left it midway. Atm it produces a new invader at every tick.
;; Another solution might be making another world, and somehow overlaying them, but then the invader wouldn't be detected
;; by missiles form the other unlesss I could somehow fuse them which is beyond me and not my priority as far as i can see.

;; Another correction would be fixing hit-ranges, becase many times missles go throught invader's without deleting each
;; other, which might be because of bad hit-boxes or how I coded detecting, under this I mean maybe the speed they go at
;; is too big and they are able to pass throught each other in which more ticks per second would be needed and decrease
;; in speed to keep the game at this pace, or just slow speed of the units and make the game play slower.

;; Plus some tests fail because I put some random variable where invaders spawn, which I didn't spend time on how to extract
;; and put in test cases so when testing both produce from the same random ranges but the exact values are different, which
;; I am fine with.

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 4)
(define MISSILE-SPEED 15)

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



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



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



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions:

;; game -> game
;; run the animation, starting with initial game state G0
;; Start with (main G0)
;; <no tests for main functions>

(define (main bs)
  (big-bang bs
    (on-tick next-game) ;advances tank, missiles, invaders, deletes and creates them
    (to-draw render-game) ;rendering
    (on-key  key-handler) ;tank direction and missile launches
    (on-release release-handler) ;tank moving, new missiles
    (stop-when end-game))) ;end when invader reaches HEIGHT


;; game -> boolean
;; produces true if any of the invaders reach HEIGHT, false otherwise
(check-expect (end-game G2) false)
(check-expect (end-game G0) false)
(check-expect (end-game G3) true)

;(define (end-game loi) true) ;stub

(define (end-game g)
  (cond [(invader-below-zero? (game-invaders g)) true]
        [else
         false]))


;; loi -> boolean
;; helper, produces true if invader reaches height, false otherwise
(define (invader-below-zero? loi)
  (cond [(empty? loi) false]
        [(<= HEIGHT (invader-y (first loi))) true]
        [else (invader-below-zero? (rest loi))]))


;; game -> game
;; advance game state by changing tank, invadre and missile x,y coordinates, deleting or creating new ones
(check-expect (next-game G0) (make-game (list (make-invader (random WIDTH) 0 (- (random 25) 12))) empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (next-game G2) (make-game (list (make-invader (random WIDTH) 0 (- (random 25) 12))
                                              (make-invader (+ 150 (/ 12 2)) (+ 100 (/ 12 2)) 12))
                                        (list (make-missile 150 (- 300 MISSILE-SPEED)))
                                        (make-tank (+ 50 TANK-SPEED) 1)))

;(define (next-game g) g) ;stub

(define (next-game g)
  (make-game (next-invader (game-invaders g) (game-missiles g) INVADE-RATE) ;; TODO INVADE-RATE constant instead of variable
             (next-missile (game-missiles g) (game-invaders g))
             (next-tank (game-tank g))))

;; loi lom tick -> loi
;; creates new invader every INVADE-RATE ticks
(check-expect (next-invader empty empty 0) empty)
(check-expect (next-invader empty empty 99) (list (make-invader (random WIDTH) 0 (- (random 25) 12))))
(check-expect (next-invader (list I1) empty 99) (list (make-invader (random WIDTH) 0 (- (random 25) 12)) I1 ))
(check-expect (next-invader (list I1) empty 50) (list I1))

;(define (next-invader loi lom t) loi) ;stub

(define (next-invader loi lom t)
  (cond [(= (tick-counter t) INVADE-RATE) (cons (make-invader (random WIDTH) 0 (- (random 25) 12)) (advance-invader loi lom))]
        [else (advance-invader loi lom)]))


;; integer integer-> integer
;; counter for new invaders
(check-expect (tick-counter 0 INVADE-RATE) 1)
(check-expect (tick-counter 50 INVADE-RATE) 51)
(check-expect (tick-counter 99 INVADE-RATE) 100)
(check-expect (tick-counter 100 INVADE-RATE) 0)
(check-expect (tick-counter 100 INVADE-RATE) 0)

(define (tick-counter i)
  (if (>= i 100) 0 (+ i 1)))


;; loi lom -> loi
;; deletes invaders which are hit with missiles from loi, advances the rest with dx
(check-expect (advance-invader empty empty) empty)
(check-expect (advance-invader (list I1 I2 I3) (list M1 M2 M3))
              (list (make-invader (+ (invader-x I2) (/ (invader-dx I2) 2))
                                  (+ (invader-y I2) (abs (/ (invader-dx I2) 2)))
                                  (invader-dx I2))
                    (make-invader (+ (invader-x I3) (/ (invader-dx I3) 2))
                                  (+ (invader-y I3) (abs (/ (invader-dx I3) 2)))
                                  (invader-dx I3))))

;(define (advance-invader loi lom) empty) ;stub

(define (advance-invader loi lom)
  (cond [(empty? loi) empty]
        [(invader-hit? (first loi) lom) (advance-invader (rest loi) lom)]
        [else (cons (make-invader (+ (invader-x (first loi)) (/ (invader-dx (first loi)) 2))
                                  (+ (invader-y (first loi)) (abs (/ (invader-dx (first loi)) 2)))
                                  (cond [(and (<= (invader-x (first loi)) 0) (>= 0 (/ (invader-dx (first loi)) 2)))
                                         (* -1 (invader-dx (first loi)))]
                                        [(and (>= (invader-x (first loi)) WIDTH) (<= 0 (/ (invader-dx (first loi)) 2)))
                                         (* -1 (invader-dx (first loi)))]
                                        [else
                                         (invader-dx (first loi))]))
                    (advance-invader (rest loi) lom))]))


;; invader lom -> true
;; true if a missile is close enough for an invader to hit, false otherwise
(check-expect (invader-hit? I1 (list M1)) false) 
(check-expect (invader-hit? I2 empty) false)
(check-expect (invader-hit? I1 (list M3)) true)
(check-expect (invader-hit? I3 (list M1 M2 M3)) false)

;(define (invader-hit? i lom) true) ;stub

(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [(and (<= (missile-x (first lom)) (+ (invader-x i) HIT-RANGE))
              (>= (missile-x (first lom)) (invader-x i))
              (<= (missile-y (first lom)) (+ (invader-y i) HIT-RANGE))
              (>= (missile-y (first lom)) (invader-y i)))
         true]
        [else
         (invader-hit? i (rest lom))]))


;; lom loi -> lom
;; deletes missiles which hit loi or reach BACKGROUND-height, advances the rest with MISSILE-SPEED
(check-expect (next-missile empty empty) empty)
(check-expect (next-missile (list M1 M2 M3) (list I1 I2 I2))
              (list (make-missile (missile-x M1) (- (missile-y M1) MISSILE-SPEED))))

;(define (next-missile lom loi) empty) ;stub

(define (next-missile lom loi)
  (cond [(empty? lom) empty]
        [(< (missile-y (first lom)) 0) (next-missile (rest lom) loi)]
        [(missile-hit? (first lom) loi) (next-missile (rest lom) loi)]
        [else (cons (make-missile (missile-x (first lom))
                                  (- (missile-y (first lom)) MISSILE-SPEED))
                    (next-missile (rest lom) loi))]))


;; missile loi -> true
;; true if an invader is close enough for a missile to hit, false otherwise
(check-expect (missile-hit? M2 (list I1 I2 I3)) true)
(check-expect (missile-hit? M2 empty) false)
(check-expect (missile-hit? M1 (list I1 I2 I3)) false)

;(define (missile-hit? m loi) true) ;stub

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [(and (<= (missile-x m) (+ (invader-x (first loi)) HIT-RANGE))
              (>= (missile-x m) (invader-x (first loi)))
              (<= (missile-y m) (+ (invader-y (first loi)) HIT-RANGE))
              (>= (missile-y m) (invader-y (first loi))))
         true]
        [else
         (missile-hit? m (rest loi))]))


;; tank -> tank
;; advances tank according to direction, if it's 0 stay in place, it stays in place if it reaches border
(check-expect (next-tank (make-tank 50 1)) (make-tank (+ 50 TANK-SPEED) 1))
(check-expect (next-tank (make-tank 50 -1)) (make-tank (- 50 TANK-SPEED) -1))
(check-expect (next-tank (make-tank 50 0)) (make-tank 50 0))
(check-expect (next-tank (make-tank 300 1)) (make-tank (- 300 (/ (image-width TANK) 2)) 1))
(check-expect (next-tank (make-tank 5 -1)) (make-tank (+ 0 (/ (image-width TANK) 2)) -1))

;(define (next-tank tank) tank) ;stub

(define (next-tank t)
  (cond [(= (tank-dir t) 0) t]
        [(and (< (- 300 (/ (image-width TANK) 2)) (+ (tank-x t) TANK-SPEED)) (= 1 (tank-dir t)))
         (make-tank (- 300 (/ (image-width TANK) 2)) 1)]
        [(= (tank-dir t) 1) (make-tank (+ (tank-x t) TANK-SPEED) 1)]
        [(and (> (+ 0 (/ (image-width TANK) 2)) (- (tank-x t) TANK-SPEED)) (= -1 (tank-dir t)))
         (make-tank (+ 0 (/ (image-width TANK) 2)) -1)]
        [(= (tank-dir t) -1) (make-tank (- (tank-x t) TANK-SPEED) -1)]))
        

;; game -> image
;; renders the current state of the game on the background
(check-expect (render-game G0) (place-image TANK (/ WIDTH 2) (- 500 (/ (image-height TANK) 2)) BACKGROUND))
(check-expect (render-game G2) (place-image INVADER 150 100
                                            (place-image MISSILE 150 300
                                                         (place-image TANK 50 (- 500 (/ (image-height TANK) 2)) BACKGROUND))))

;(define (render-game g) empty) ;stub

(define (render-game g)
  (render-missiles (game-missiles g) (game-invaders g) (game-tank g)))


;;listofmissiles listofinvaders tank -> image
;; render the missiles onthe the image with tanks and invaders
(check-expect (render-missiles empty (game-invaders G0) (game-tank G0))
              (render-invaders (game-invaders G0) (game-tank G0)))

;(define (render-missile lom) empty) ;stub

(define (render-missiles lom loi t)
  (cond [(empty? lom) (render-invaders loi t)]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) loi t))]))


;;listofinvaders tank -> image
;; render the invaders onto the scene with tank
(check-expect (render-invaders empty (game-tank G0)) (render-tank (game-tank G0)))
(check-expect (render-invaders (cons I1 empty) (game-tank G0))
              (place-image INVADER 150 100 (render-tank (game-tank G0))))
(check-expect (render-invaders (list I1 I2 I3) (game-tank G0))
              (place-image INVADER 150 100
                           (place-image INVADER 150 HEIGHT
                                        (place-image INVADER 150 (+ HEIGHT 10) (render-tank (game-tank G0))))))

;(define (render-invaders loi t) empty) ;stub

(define (render-invaders loi t)
  (cond [(empty? loi) (render-tank t)]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) t))]))


;; tank -> image
;; render the tank
(define (render-tank t)
  (place-image TANK (tank-x t) (- 500 (/ (image-height TANK) 2)) BACKGROUND))


;; game keyevent -> game
;; change tank direction if left or right is pressed, create new missile on left mouseclick pressed
(check-expect (key-handler G0 "k") (make-game empty
                                              empty
                                              (make-tank (/ WIDTH 2) 1)))
(check-expect (key-handler G0 "a") (make-game empty
                                                 empty
                                                 (make-tank (/ WIDTH 2) -1)))
(check-expect (key-handler G0 "w") (make-game empty
                                                       (cons (make-missile 150  (- 500 (+ (image-height TANK)
                                                                                    (image-height MISSILE))))
                                                             empty)
                                                       (make-tank (/ WIDTH 2) 1)))
(check-expect (key-handler (make-game empty (cons (make-missile 150 300) empty) (make-tank (/ WIDTH 2) 1))
                           "w")
              (make-game empty (cons (make-missile (/ WIDTH 2) (- 500 (+ (image-height TANK)
                                                                         (image-height MISSILE))))
                                     (cons (make-missile 150 300)
                                           empty)) (make-tank (/ WIDTH 2) 1)))

;(define (key-handler g k) g) ;stub

(define (key-handler g k)
  (cond [(key=? k "a") (make-game (game-invaders g)
                                     (game-missiles g)
                                     (make-tank (tank-x (game-tank g)) -1))]
        [(key=? k "d") (make-game (game-invaders g)
                                      (game-missiles g)
                                      (make-tank (tank-x (game-tank g)) 1))]
        [(key=? k "w") (make-game (game-invaders g)
                                           (cons (make-missile (tank-x (game-tank g))
                                                               (- 500 (+ (image-height TANK)
                                                                         (image-height MISSILE)))) (game-missiles g))
                                           (make-tank (tank-x (game-tank g))
                                                      (tank-dir (game-tank g))))]
        [else
         g]))


;; game releaseevent -> game
;; tanks direction changes to 0 is left or right is released
(check-expect (release-handler G0 "k") (make-game empty
                                                  empty
                                                  (make-tank (/ WIDTH 2) 1)))
(check-expect (release-handler G0 "a") (make-game empty
                                                     empty
                                                     (make-tank (/ WIDTH 2) 0)))
(check-expect (release-handler G0 "d") (make-game empty
                                                      empty
                                                      (make-tank (/ WIDTH 2) 0)))

;(define (release-handler g k) g) ;stub

(define (release-handler g k)
  (cond [(key=? "a" k)(make-game (game-invaders g)
                                    (game-missiles g)
                                    (make-tank (tank-x (game-tank g)) 0))]
        [(key=? "d" k)(make-game (game-invaders g)
                                     (game-missiles g)
                                     (make-tank (tank-x (game-tank g)) 0))]
        [else
         g]))

