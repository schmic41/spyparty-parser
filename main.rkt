#lang racket/gui

(require "helpers.rkt")
(require "parser.rkt")
(require racket/match)
(require racket/date)
(require racket/string)
(require (only-in srfi/19 string->date))

; Default for date->string
(date-display-format 'rfc2822)

; Create the main window
(define main-frame (new frame% [label "SpyParty Replay Parser"] [min-width 1000] [min-height 900]))
(send main-frame center 'both)

; Behind the scenes
(define (send-all-replays list-of-good-replays)
  ; Named let to enable custom control flow for venues and dates
  (let recur ([t-list list-of-good-replays]
              [spy-list '()]
              [sniper-list '()]
              [outcome-list '()]
              [venue-list '()]
              [date-list '()])
    (if (empty? t-list)
        (send replay-select set spy-list sniper-list outcome-list venue-list date-list)
        (recur (rest t-list)
               (cons (get-spy-name (first t-list)) spy-list)
               (cons (get-sniper-name (first t-list)) sniper-list)
               (cons (replay-result (first t-list)) outcome-list)
               (cons (if (or (equal? "Teien" (replay-venue (first t-list)))
                             (equal? "Aquarium" (replay-venue (first t-list))))
                         (~a (replay-venue (first t-list)) " (" (get-variant (first t-list)) ")")
                         (replay-venue (first t-list))) venue-list)
               (cons (date->string
                      (seconds->date
                       (replay-start-time (first t-list)))
                      #t)
                     date-list)))))

(define *master-replay-list* null)
(define *temp-replay-list* null)
(define default-columns (vector 0 0 0 0 0)) ; Modalty for sort order

; The main select section, implemented a list-bx
(define replay-select (new list-box%
                           [parent main-frame]
                           [label #f]
                           [choices '()]
                           [callback
                            (lambda (list-box control-event)
                              (let* ([event-type (send control-event get-event-type)]
                                     ; Wraparound when to avoid errors on single/double click
                                     [c (when (not (equal? event-type 'list-box))
                                          (send control-event get-column))])
                                (match (list event-type c)
                                  [(list 'list-box-column 0)
                                   (if (= 0 (vector-ref default-columns c))
                                       (begin
                                         (vector-set! default-columns c 1)
                                         (set! *temp-replay-list* (sort *temp-replay-list* string>?
                                                                        #:key get-spy-name))
                                         (send-all-replays *temp-replay-list*))
                                       (begin
                                         (vector-set! default-columns c 0)
                                         (set! *temp-replay-list* (sort *temp-replay-list* string<?
                                                                        #:key get-spy-name))
                                         (send-all-replays *temp-replay-list*)))]
                                  [(list 'list-box-column 1)
                                   (if (= 0 (vector-ref default-columns c))
                                       (begin
                                         (vector-set! default-columns c 1)
                                         (set! *temp-replay-list*
                                               (sort *temp-replay-list* string>?
                                                     #:key get-sniper-name))
                                         (send-all-replays *temp-replay-list*))
                                       (begin
                                         (vector-set! default-columns c 0)
                                         (set! *temp-replay-list*
                                               (sort *temp-replay-list* string<?
                                                     #:key get-sniper-name))
                                         (send-all-replays *temp-replay-list*)))]
                                  [(list 'list-box-column 2)
                                   (if (= 0 (vector-ref default-columns c))
                                       (begin
                                         (vector-set! default-columns c 1)
                                         (set! *temp-replay-list*
                                               (sort *temp-replay-list* string>?
                                                     #:key replay-result))
                                         (send-all-replays *temp-replay-list*))
                                       (begin
                                         (vector-set! default-columns c 0)
                                         (set! *temp-replay-list*
                                               (sort *temp-replay-list* string<?
                                                     #:key replay-result))
                                         (send-all-replays *temp-replay-list*)))]
                                  [(list 'list-box-column 3)
                                   (if (= 0 (vector-ref default-columns c))
                                       (begin
                                         (vector-set! default-columns c 1)
                                         (set! *temp-replay-list*
                                               (sort *temp-replay-list* string>?
                                                     #:key replay-venue))
                                         (send-all-replays *temp-replay-list*))
                                       (begin
                                         (vector-set! default-columns c 0)
                                         (set! *temp-replay-list*
                                               (sort *temp-replay-list* string<?
                                                     #:key replay-venue))
                                         (send-all-replays *temp-replay-list*)))]
                                  [(list 'list-box-column 4)
                                   (if (= 0 (vector-ref default-columns c))
                                       (begin
                                         (vector-set! default-columns c 1)
                                         (set! *temp-replay-list* (sort *temp-replay-list* >
                                                                        #:key replay-start-time))
                                         (send-all-replays *temp-replay-list*))
                                       (begin
                                         (vector-set! default-columns c 0)
                                         (set! *temp-replay-list* (sort *temp-replay-list* <
                                                                        #:key replay-start-time))
                                         (send-all-replays *temp-replay-list*)))]
                                  [(list _ _) null])))]
                           [style (list 'extended 'column-headers 'clickable-headers 'reorderable-headers)]
                           [min-height 500]
                           [columns (list "Spy"
                                          "Sniper"
                                          "Outcome"
                                          "Venue"
                                          "Date")]))

; Default column widths for presentation
(send replay-select set-column-width 0 200 0 10000)
(send replay-select set-column-width 1 200 0 10000)
(send replay-select set-column-width 2 150 0 10000)
(send replay-select set-column-width 3 175 0 10000)
(send replay-select set-column-width 4 200 0 10000)

; Hardcoded for testing DELETE THIS
(define test-dir "C:\\Users\\Cameron\\Desktop\\SpyParty Parser\\Test Replays")

(let-values ([(good bad) (get-games test-dir)])
  (when (not (empty? bad))
    (message-box "Skipped Files" "Hello" main-frame 'ok))
  (set! *master-replay-list* good)
  (set! *temp-replay-list* good)
  (send-all-replays good)
  null)

; Menu items
(define menu-bar (new menu-bar% (parent main-frame)))

(define file-menu (new menu% [label "File"] [parent menu-bar]))
(define open-folder (new menu-item% [label "Open Folder"] [parent file-menu]
                         (callback (lambda (x y)
                                     (let-values ([(good bad) (get-games
                                                               (get-directory "Open File Folder"
                                                                              main-frame))])
                                       (when (not (empty? bad))
                                         (message-box "Skipped Files" "Hello" main-frame 'ok))
                                       (set! *master-replay-list* good)
                                       (set! *temp-replay-list* good)
                                       (send-all-replays *temp-replay-list*))))))

(define reset (new menu-item% [label "Reset"] [parent file-menu]
                   [callback (lambda (parent date-menu)
                               (send replay-select set null null null null null)
                               (set! *temp-replay-list* *master-replay-list*)
                               (send-all-replays *temp-replay-list*))]))

(define filter-menu (new menu% [label "Filter"] [parent menu-bar]))
(define filter-submenu (new menu-item% [label "Filter"] [parent filter-menu]
                            [callback
                             (lambda (parent menu-bar)
                               (define filter-menu (new dialog% [label "Filter"]))
                               (define layout-frame (new horizontal-panel% [parent filter-menu]))
                               (define filter-p1 (new text-field% (parent layout-frame) (label "Username ")))
                               (define filter-role (new choice% (parent layout-frame)
                                                        (choices (list "Spy or Sniper"
                                                                       "Spy"
                                                                       "Sniper"))
                                                        (label " as  ")))
                               (define filter-p2 (new text-field% (parent layout-frame) (label " versus  ")))
                               (define filter-venue (new choice% (label " on venue  ") (parent layout-frame) (selection 0)
                                                         [choices (list "Any"
                                                                        "Veranda"
                                                                        "Courtyard"
                                                                        "Library"
                                                                        "Balcony"
                                                                        "Gallery"
                                                                        "Terrace"
                                                                        "Moderne"
                                                                        "Teien"
                                                                        "Aquarium"
                                                                        "BvB High-Rise"
                                                                        "BvB Ballroom"
                                                                        "Ballroom"
                                                                        "High-Rise"
                                                                        "Old-Art Gallery"
                                                                        "Old-Art Courtyard 2"
                                                                        "Old-Art Panopticon"
                                                                        "Old-Art Veranda"
                                                                        "Old-Art Balcony"
                                                                        "Crowded Pub"
                                                                        "Pub"
                                                                        "Old-Art Ballroom"
                                                                        "Old-Art Courtyard 1"
                                                                        "Double Modern"
                                                                        "Modern")]))
                               (define filter-time1 (new text-field% [label " between  "] [parent layout-frame] [init-value (date->string (seconds->date 0) #t)]))
                               (define filter-time2 (new text-field% [label " and  "] [parent layout-frame] [init-value (date->string (current-date) #t)]))
                               (define filter-go
                                 (new button%
                                      (parent layout-frame)
                                      (label "Filter")
                                      (callback
                                       (λ (button control-element)
                                         (let* ([p1-name (send filter-p1 get-value)]
                                                [role (send filter-role get-string-selection)]
                                                [p2-name (send filter-p2 get-value)]
                                                [venue (if (equal? "Any" (send filter-venue get-string-selection))
                                                           #f
                                                           (send filter-venue get-string-selection))]
                                                [parse-date (lambda (s)
                                                              (match (date-display-format)
                                                                ['iso-8601 (string->date s
                                                                                         "~Y-~m-~dT~H:~M:~S")]
                                                                ['rfc2822 (string->date s
                                                                                        "~a, ~d ~b ~Y ~H:~M:~S ~z")]))]
                                                [start-time (parse-date (send filter-time1 get-value))]
                                                [end-time (parse-date (send filter-time2 get-value))]
                                                [temp-filter (λ (a)
                                                               (and
                                                                (cond
                                                                  ((and (equal? "Spy" role)
                                                                        (non-empty-string? p1-name))
                                                                   (equal? (get-spy-name a) p1-name))
                                                                  ((and (equal? "Sniper" role)
                                                                        (non-empty-string? p1-name))
                                                                   (equal? (get-sniper-name a) p1-name))
                                                                  ((and (equal? "Spy or Sniper" role)
                                                                        (non-empty-string? p1-name))
                                                                   (or (equal? (get-sniper-name a) p1-name)
                                                                       (equal? (get-spy-name a) p1-name))))
                                                                (if (non-empty-string? p2-name)
                                                                    (or (equal? (get-sniper-name a) p2-name)
                                                                        (equal? (get-spy-name a) p2-name))
                                                                    #t)
                                                                (if (not venue)
                                                                    #t
                                                                    (equal? (replay-venue a) venue))
                                                                (>= (replay-start-time a) (date->seconds start-time))
                                                                (<= (replay-start-time a) (date->seconds end-time))))])
                                           (send replay-select set null null null null null)
                                           (let ([temp-sorted (filter temp-filter *master-replay-list*)])
                                             (set! *temp-replay-list* temp-sorted)
                                             (send-all-replays *temp-replay-list*)))))))
                               (send filter-menu show #t))]))

(define date-menu (new menu% (label "Date") (parent menu-bar)))
(define (helper-callback s)
  (lambda (item event) (date-display-format s)
     (send-all-replays *temp-replay-list*)))

; TODO: format strings for all of these

;(define am (new menu-item% (label "American") (parent date-menu) (callback (helper-callback 'american))))
;(define ch (new menu-item% (label "Chinese") (parent date-menu) (callback (helper-callback 'chinese))))
;(define ger (new menu-item% (label "German") (parent date-menu) (callback (helper-callback 'german))))
;(define ind (new menu-item% (label "Indian") (parent date-menu) (callback (helper-callback 'indian))))
;(define iri (new menu-item% (label "Irish") (parent date-menu) (callback (helper-callback 'irish))))
(define iso (new menu-item% (label "ISO-8601") (parent date-menu) (callback (helper-callback 'iso-8601))))
(define rfc (new menu-item% (label "RFC-2822") (parent date-menu) (callback (helper-callback 'rfc2822))))
;(define jul (new menu-item% (label "Julian") (parent date-menu) (callback (helper-callback 'julian))))

; Show the window
(send main-frame show #t)
