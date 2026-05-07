#lang racket
(require racket/string
         racket/gui/base)

; -----------------------------------------------
; STRUCTS
; define the data shapes for game world
; -----------------------------------------------

(struct game (rooms) #:transparent)
(struct room-v (name desc items monsters exits power) #:transparent)
(struct item-v (name desc type) #:transparent)
(struct monster-v (name hp) #:transparent)
(struct exit-v (direction destination key) #:transparent)

(define (get-room rooms name)
  (cdr (assoc name rooms)))

;; -----------------------------------------------
;; GRAPHICS
;; one image window, smaller centered images
;; -----------------------------------------------

(define image-frame #f)
(define image-canvas #f)
(define current-bitmap #f)

(define WINDOW-WIDTH 700)
(define WINDOW-HEIGHT 500)
(define IMAGE-SCALE 0.5) ; change to 0.5 smaller, 0.8 bigger

(define (show-room-image room-name)
  (define path (string-append "images/" room-name ".png"))

  (if (file-exists? path)
      (begin
        (set! current-bitmap (make-object bitmap% path))

        (if image-frame
            (send image-canvas refresh-now)
            (begin
              (set! image-frame
                    (new frame%
                         [label "Dungeon View"]
                         [width WINDOW-WIDTH]
                         [height WINDOW-HEIGHT]))

              (set! image-canvas
                    (new canvas%
                         [parent image-frame]
                         [min-width WINDOW-WIDTH]
                         [min-height WINDOW-HEIGHT]
                         [paint-callback
                          (lambda (canvas dc)
                            (when current-bitmap
                              (define img-w (send current-bitmap get-width))
                              (define img-h (send current-bitmap get-height))

                              (define draw-w (* img-w IMAGE-SCALE))
                              (define draw-h (* img-h IMAGE-SCALE))

                              (define x (/ (- WINDOW-WIDTH draw-w) 2))
                              (define y (/ (- WINDOW-HEIGHT draw-h) 2))

                              (send dc set-scale IMAGE-SCALE IMAGE-SCALE)
                              (send dc draw-bitmap current-bitmap
                                    (/ x IMAGE-SCALE)
                                    (/ y IMAGE-SCALE))
                              (send dc set-scale 1 1)))]))

              (send image-frame show #t))))
      (displayln (string-append "Missing image: " path))))

; -----------------------------------------------
; PLAY
; entry point called by the expander
; -----------------------------------------------

(define (play world)
  (show-room-image "map")
  (displayln "==========================================")
  (displayln "       WELCOME TO THE DUNGEON!")
  (displayln "==========================================")
  (displayln "Study the map. Press ENTER when you are ready to begin.")
  (read-line)

  (define rooms (game-rooms world))
  (define start-name (car (car rooms)))
  (game-loop rooms start-name 10 '()))

; -----------------------------------------------
; GAME LOOP
; called when player enters new room or after an action
; -----------------------------------------------

(define (game-loop rooms current player-power inventory)
  (define r (get-room rooms current))
  (show-room-image current)

  ; ---- WIN CONDITION ----
  (when (null? (room-v-exits r))
    (displayln "==========================================")
    (displayln (room-v-desc r))
    (displayln "==========================================")
    (displayln "          ★ YOU WIN! ★")
    (printf "    You escaped with a power of ~a!\n" player-power)
    (displayln "==========================================")
    (exit))

  ; ---- PRINT ROOM INFO ----
  (displayln "==========================================")
  (printf "  ~a~aPower: ~a\n"
          (string-upcase (room-v-name r))
          (make-string (max 1 (- 28 (string-length (room-v-name r)))) #\space)
          player-power)
  (displayln "==========================================")
  (printf "  ~a\n\n" (room-v-desc r))

  (printf "  Exits: ~a\n"
          (string-join
           (map (lambda (e) (exit-v-direction e))
                (room-v-exits r))
           " | "))

  (when (not (null? (room-v-items r)))
    (printf "  Items:    ~a\n"
            (string-join (map item-v-name (room-v-items r)) ", ")))

  (when (not (null? (room-v-monsters r)))
    (printf "  Monsters: ~a\n"
            (string-join
             (map (lambda (m)
                    (format "~a (hp: ~a)"
                            (monster-v-name m)
                            (monster-v-hp m)))
                  (room-v-monsters r))
             ", ")))

  (printf "  Inventory: ~a\n"
          (if (null? inventory)
              "empty"
              (string-join (map item-v-name inventory) ", ")))

  (displayln "==========================================")

  ; ---- ITEM PICKUP PHASE ----
  (define-values (power-after-items new-inventory)
    (cond
      [(null? (room-v-items r))
       (values player-power inventory)]
      [else
       (displayln "Type 'take <item>' or 'continue'")
       (display "> ")
       (define item-input (read-line))

       (if (and (>= (string-length item-input) 5)
                (equal? (substring item-input 0 5) "take "))
           (let* ([item-name (substring item-input 5)]
                  [found-item
                   (findf (lambda (i)
                            (equal? (item-v-name i) item-name))
                          (room-v-items r))])
             (if found-item
                 (values
                  (+ player-power
                     (if (equal? (item-v-type found-item) "weapon")
                         5
                         0))
                  (cons found-item inventory))
                 (values player-power inventory)))
           (values player-power inventory))]))

  ; ---- COMBAT PHASE ----
  (define power-after-combat
    (if (null? (room-v-monsters r))
        power-after-items
        (let ([m (car (room-v-monsters r))])
          (printf "A ~a blocks your path! (hp: ~a)\n"
                  (monster-v-name m)
                  (monster-v-hp m))
          (display "fight or run? > ")
          (define choice (read-line))
          (when (equal? choice "quit") (displayln "Goodbye!") (exit))

          (cond
            ; --- RUN ---
            [(equal? choice "run")
             (displayln "You run! Which way?")
             (for ([e (room-v-exits r)])
               (printf "Exit: ~a\n" (exit-v-direction e)))
             (display "> ")
             (define run-dir (read-line))

             (define run-dest #f)
             (for ([e (room-v-exits r)])
               (when (equal? (exit-v-direction e) run-dir)
                 (set! run-dest (exit-v-destination e))))

             (if run-dest
                 (game-loop rooms run-dest power-after-items new-inventory)
                 (begin
                   (displayln "Can't go that way!")
                   (game-loop rooms current power-after-items new-inventory)))]

            ; --- FIGHT ---
            [(equal? choice "fight")
             (if (>= power-after-items (string->number (monster-v-hp m)))
                 (begin
                   (displayln "You defeated the monster! Power +5")
                   (+ power-after-items 5))
                 (begin
                   (displayln "You are too weak... YOU DIED!")
                   (exit)))]

            ; --- INVALID ---
            [else
             (displayln "Type 'fight' or 'run'.")
             (game-loop rooms current power-after-items new-inventory)]))))

  ; ---- MOVEMENT PHASE (WITH LOCKED DOORS) ----
  (displayln "Where do you go?")
  (display "> ")
  (define input (read-line))

  (define next-exit
    (findf (lambda (e)
             (equal? (exit-v-direction e) input))
           (room-v-exits r)))

  (if next-exit
      (let ([required-key (exit-v-key next-exit)])
        (if (or (not required-key)
                (findf (lambda (i)
                         (equal? (item-v-name i)
                                 (if (symbol? required-key)
                                     (symbol->string required-key)
                                     required-key)))
                       new-inventory))
            (game-loop rooms
                       (exit-v-destination next-exit)
                       power-after-combat
                       new-inventory)
            (begin
              (displayln "That door is locked.")
              (game-loop rooms current power-after-combat new-inventory))))
      (begin
        (displayln "Can't go that way.")
        (game-loop rooms current power-after-combat new-inventory))))

(provide (all-defined-out))
