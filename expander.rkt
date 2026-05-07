#lang br/quicklang

(require racket/match
         brag/support
         "game.rkt")

;; the whole parsed program will first be handed to dungeon-module-begin

(provide (rename-out [dungeon-module-begin #%module-begin]))
(provide (matching-identifiers-out
          #rx"^(program|room|room-body|room-element|desc|item|item-body|item-field|type|monster|exit|power|key)$"
          (all-defined-out)))

;; small wrapper values used while assembling room and items.

(struct desc-node (text) #:transparent)
(struct item-node (value) #:transparent)
(struct monster-node (value) #:transparent)
(struct exit-node (value) #:transparent)
(struct power-node (value) #:transparent)
(struct type-node (value) #:transparent)
(struct key-node (value) #:transparent)

;; HELPER: remove surrounding quotes from strings

(define (unquote s)
  (if (and (string? s)
           (>= (string-length s) 2)
           (string=? (substring s 0 1) "\"")
           (string=? (substring s (sub1 (string-length s))) "\""))
      (substring s 1 (sub1 (string-length s)))
      s))

;; BUILD AN ITEM FROM ITS FIELDS

(define (build-item name fields)
  (define desc-text #f)
  (define item-type #f)

  (for ([f fields])
    (match f
      [(desc-node txt)
       (set! desc-text (unquote txt))]
      [(type-node t)
       (set! item-type t)]
      [_ (error 'build-item "invalid item field: ~a" f)]))

  (item-v name desc-text item-type))

;; BUILD A ROOM FROM ITS ELEMENTS

(define (build-room name elements)
  (define desc-text #f)
  (define items '())
  (define monsters '())
  (define exits '())
  (define room-power #f)

  (for ([el elements])
    (match el
      [(desc-node txt)
       (set! desc-text (unquote txt))]
      [(item-node i)
       (set! items (cons i items))]
      [(monster-node m)
       (set! monsters (cons m monsters))]
      [(exit-node e)
       (set! exits (cons e exits))]
      [(power-node p)
       (set! room-power p)]
      [_ (error 'build-room "invalid room element: ~a" el)]))

  (room-v name
          desc-text
          (reverse items)
          (reverse monsters)
          (reverse exits)
          room-power))

;; PARSE TREE EXPANDERS

;; ---room---

(define-macro (room NAME BODY)
  (with-pattern ([ROOM-ID (prefix-id "room-" #'NAME #:source #'NAME)])
    (syntax/loc caller-stx
      (define ROOM-ID
        (build-room NAME BODY)))))

(define-macro (room-body ELEMENT ...)
  #'(list ELEMENT ...))

(define-macro (room-element ELEMENT)
  #'ELEMENT)

(define-macro (item-body FIELD ...)
  #'(list FIELD ...))

(define-macro (item-field FIELD)
  #'FIELD)

;; ---desc---

(define-macro (desc STR)
  #'(desc-node STR))

;; ---item---

(define-macro (item NAME BODY)
  #'(item-node (build-item NAME BODY)))

;; ---type---

(define-macro (type NAME)
  #'(type-node NAME))

;; ---key---

(define-macro (key NAME)
  #'NAME)

;; ---monster---

(define-macro (monster NAME . REST)
  (define rest-list (syntax->list #'REST))
  (define hp (if (null? rest-list)
                 25
                 (string->number (syntax->datum (car rest-list)))))
  (define power (if (< (length rest-list) 2)
                    5
                    (string->number (syntax->datum (cadr rest-list)))))
  #`(monster-node (monster-v NAME #,hp #,power)))

;; ---exit---

 (define-macro (exit DIR DEST . REST)
  (with-pattern ([KEY (if (null? (syntax->list #'REST))
                          #'#f
                          (car (syntax->list #'REST)))])
    #'(exit-node (exit-v DIR DEST KEY))))

;; ---power---

(define-macro (power N)
  #'(power-node N))

;; -----dungeon-module-begin-----

(define-macro (dungeon-module-begin (program ROOM ...))
  (with-pattern
      ([((room ROOM-NAME ELEMENT ...) ...) #'(ROOM ...)]
       [(ROOM-ID ...) (prefix-id "room-" #'(ROOM-NAME ...))])
    #'(#%module-begin
       ROOM ...
       (define game-world
         (game (list (cons ROOM-NAME ROOM-ID) ...)))
       (play game-world)
       (provide game-world))))
