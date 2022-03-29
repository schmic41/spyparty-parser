#lang racket/base

(require racket/gui)
(require "helpers.rkt")
(require "parser.rkt")
(require "glicko.rkt")
(require racket/match)
(require racket/date)
(require racket/string)
(require racket/vector)
(require net/base64)
(require racket/stream)
(require plot)
(require (only-in srfi/19 string->date))

; Default for date->string
(date-display-format 'rfc2822)

; Create the main window
(define main-frame (new frame% [label "SpyParty Replay Viewer"] [min-width 1000] [min-height 900]))
(send main-frame center 'both)

; Show the window
(send main-frame show #t)

(define (show-single-match r)
  (define temp-frame (new frame% [label "Replay"] [parent main-frame] [min-width 700] [min-height 500]))
  (send temp-frame center 'both)
  (send temp-frame show #t)
  (define layout (new vertical-panel% [parent temp-frame]))
  (define text (new text% [auto-wrap #t]))
  (send text hide-caret #t)
  (define viewer (new editor-canvas% [parent layout] [editor text]))
  (send text insert (~a "Spy: " (get-spy-name r) "\n"
                        "Sniper: " (get-sniper-name r) "\n"
                        "Game Version: " (replay-game-version r) "\n"
                        "Venue: "
                        (if (or (equal? "Teien" (replay-venue r))
                                (equal? "Aquarium" (replay-venue r)))
                            (~a (replay-venue r) " (" (get-variant r) ")")
                            (replay-venue r)) "\n"
                        "Duration: " (~r (/ (replay-duration r) 60) #:precision 2) " min\n"
                        "Start: " (date->string (seconds->date (replay-start-time r)) #t) "\n"
                        "Game Type: " (replay-game-type r) "\n"
                        "Result: " (replay-result r) "\n"
                        (if (equal? 6 (replay-file-version r))
                            (~a "Guest Count: " (replay-guest-count r) "\n")
                            "")
                        "Filename: " (replay-filename r) "\n"
                        "File Version: " (replay-file-version r) "\n"
                        "Protocol Version: " (replay-protocol-version r) "\n"
                        "UUID: " (bytes->string/utf-8 (base64-encode (replay-uuid r) ""))))
  (let* ([all-missions (list "Bug Ambassador"
                             "Contact Double Agent"
                             "Transfer Microfilm"
                             "Swap Statue"
                             "Inspect Statues"
                             "Seduce Target"
                             "Purloin Guest List"
                             "Fingerprint Ambassador")]
         [s-missions (filter-not void? (if (not (equal? (substring (replay-game-type r) 0 1) "p"))
                                           (replay-selected-missions r)
                                           (replay-picked-missions r)))]
         [completed (filter-not void? (replay-completed-missions r))])
    (for/list ([m (in-list s-missions)])
      (new check-box% [parent layout] [label m] [value (set-member? completed m)])))
  null)

(define replay-list-box%
  (class list-box%
    (super-new)
    (define replays null)

    (define/public (get-replays)
      replays)

    (define sort-modal (vector #f #f #f #f #f))

    (define/public (get-replay i)
      (vector-ref replays (- (- (vector-length replays) 1) i)))

    (define/public (update-replays!)
      (let recur ([i 0]
              [t-vector replays]
              [spy-list '()]
              [sniper-list '()]
              [outcome-list '()]
              [venue-list '()]
              [date-list '()])
        (if (= i (vector-length t-vector))
            (send this set spy-list sniper-list outcome-list venue-list date-list)
            (recur (+ i 1)
              t-vector
              (cons (get-spy-name (vector-ref t-vector i)) spy-list)
              (cons (get-sniper-name (vector-ref t-vector i)) sniper-list)
              (cons (replay-result (vector-ref t-vector i)) outcome-list)
              (cons (if (or (equal? "Teien" (replay-venue (vector-ref t-vector i)))
                            (equal? "Aquarium" (replay-venue (vector-ref t-vector i))))
                        (~a (replay-venue (vector-ref t-vector i)) " (" (get-variant (vector-ref t-vector i)) ")")
                        (replay-venue (vector-ref t-vector i))) venue-list)
              (cons (date->string
                     (seconds->date
                      (replay-start-time (vector-ref t-vector i)))
                     #t)
                    date-list)))))

    (define/public (set-replays! vec)
      (set! replays vec)
      (update-replays!))
    
    (define/public (get-modal! i)
      (let ([temp (vector-ref sort-modal i)])
        (vector-set! sort-modal i (not temp))
        temp))

    (define/public (sort-replay! comparator key)
      (vector-sort! replays comparator #:key key)
      (update-replays!))))

(define replay-select-callback
  (lambda (list-box control-event)
    (let* ([event-type (send control-event get-event-type)]
           ; Wraparound when to avoid errors on single click
           [c (if (equal? event-type 'list-box)
                  null
                  (if (equal? event-type 'list-box-dclick)
                      (send list-box get-selections)
                      (send control-event get-column)))])
      (match (list event-type c)
        [(list 'list-box-column 0)
         (if (send list-box get-modal! c)
             (send list-box sort-replay! string>? get-spy-name)
             (send list-box sort-replay! string<? get-spy-name))]
        [(list 'list-box-column 1)
         (if (send list-box get-modal! c)
             (send list-box sort-replay! string>? get-sniper-name)
             (send list-box sort-replay! string<? get-sniper-name))]
        [(list 'list-box-column 2)
         (if (send list-box get-modal! c)
             (send list-box sort-replay! string>? replay-result)
             (send list-box sort-replay! string<? replay-result))]
        [(list 'list-box-column 3)
         (if (send list-box get-modal! c)
             (send list-box sort-replay! string>? replay-venue)
             (send list-box sort-replay! string<? replay-venue))]
        [(list 'list-box-column 4)
         (if (send list-box get-modal! c)
             (send list-box sort-replay! > replay-start-time)
             (send list-box sort-replay! < replay-start-time))]
        [(list 'list-box-dclick _) (show-single-match (send list-box get-replay (car c)))]
        [(list _ _) null]))))

(define replay-select (new replay-list-box%
                           [parent main-frame]
                           [label #f]
                           [choices '()]
                           [callback replay-select-callback]
                           [style (list 'extended 'column-headers 'clickable-headers 'reorderable-headers)]
                           [min-height 500]
                           [columns (list "Spy"
                                          "Sniper"
                                          "Outcome"
                                          "Venue"
                                          "Date")]))

; Menu items
(define menu-bar (new menu-bar% (parent main-frame)))

(define file-menu (new menu% [label "File"] [parent menu-bar]))
(define open-file (new menu-item% [label "Open File(s)"] [parent file-menu]
                       [shortcut-prefix (list 'ctl)]
                       [shortcut #\O]
                       [callback (lambda (x y)
                                   (let ([tempfiles (get-file-list
                                                     "Open File(s)"
                                                     main-frame
                                                     #f
                                                     #f
                                                     #f
                                                     null
                                                     (list
                                                      (list "REPLAY File" "*.replay")))])
                                     ; Returns #f if dialog is canceled, when is guard
                                     (when tempfiles
                                       (begin-busy-cursor)
                                       (let*-values ([(good bad) (get-games1 tempfiles)]
                                                     [(good1) (list->vector good)])
                                         (when (not (empty? bad))
                                           (message-box "Skipped Files" (~a bad) main-frame 'ok))
                                         (send replay-select set-replays! good1)
                                         (end-busy-cursor)))))]))

(define open-folder (new menu-item% [label "Open Directory"] [parent file-menu]
                         (callback (lambda (x y)
                                     (begin-busy-cursor)
                                     (let*-values ([(dir) (get-directory
                                                           "Open Directory"
                                                           main-frame)]
                                                   [(good bad) (get-games dir)]
                                                   [(good1) (list->vector good)])
                                       (when (and (list? bad) (not (empty? bad)))
                                         (message-box "Skipped Files" (~a bad) main-frame (list 'ok 'stop)))
                                       (when (equal? 'empty bad)
                                         (message-box "Error" "No replay files found in this directory." main-frame (list 'ok 'stop)))
                                       (send replay-select set-replays! good1)
                                       (end-busy-cursor))))
                         [shortcut-prefix (list 'ctl)]
                         [shortcut #\D]))

(define stats (new menu% [parent menu-bar] [label "Stats"]))
(define modal1 (vector #t))

(define stats-callback
  (lambda (parent y)
    (when (not (empty? (send replay-select get-replays)))
      (let* ([players (make-hash)]
             [player-history (make-hash)]
             [sorted-vec (vector-filter (lambda (x) (not (equal? (replay-result x)
                                                                 "In-Progress")))
                                        (vector-sort (send replay-select get-replays) <
                                                     #:key replay-start-time))]
             [first-date (replay-start-time (vector-ref sorted-vec 0))]
             [step 604800])
        (define w (replay-start-time (vector-ref sorted-vec 0)))
        (let loop ([v (sequence->stream (in-vector sorted-vec))])
          (if (stream-empty? v)
              (begin
                (set! w (+ w step))
                (for ([(player-name player-struct) (in-hash players)])
                  (let-values ([(new-r new-rd new-σ) (glicko player-struct)])
                    (hash-set! player-history player-name
                               (cons (list
                                      0
                                      w
                                      new-r
                                      new-rd
                                      new-σ)
                                     (hash-ref player-history player-name)))
                    (hash-set! players player-name
                               (player new-r new-rd new-σ
                                       '()
                                       '()
                                       '()
                                       '())))))
              (if (>= (replay-start-time (stream-first v)) w)
                  (loop (begin
                          (set! w (+ w step))
                          (for ([(player-name player-struct) (in-hash players)])
                            (let-values ([(new-r new-rd new-σ) (glicko player-struct)])
                              (hash-set! player-history player-name
                                         (cons (list
                                                0
                                                w
                                                new-r
                                                new-rd
                                                new-σ)
                                               (hash-ref player-history player-name)))
                              (hash-set! players player-name
                                         (player new-r new-rd new-σ
                                                 '()
                                                 '()
                                                 '()
                                                 '()))))
                          v))
                  (loop (let* ([game (stream-first v)]
                               [winner (match (replay-result game)
                                         [(or "Missions Win" "Civilian Shot") (get-spy-name game)]
                                         [(or "Time Out" "Spy Shot") (get-sniper-name game)])]
                               [loser (match (replay-result game)
                                        [(or "Missions Win" "Civilian Shot") (get-sniper-name game)]
                                        [(or "Time Out" "Spy Shot") (get-spy-name game)])]
                               [spy (get-spy-name game)]
                               [sniper (get-sniper-name game)])
                          (when (not (hash-ref players winner #f))
                            (hash-set! player-history winner
                                       (list (list 1 (replay-start-time game) 1500.0 350.0 0.06)))
                            (hash-set! players winner
                                       (player 1500.0 350.0 0.06
                                               '()
                                               '()
                                               '()
                                               '())))
                          (when (not (hash-ref players loser #f))
                            (hash-set! player-history loser
                                       (list (list 1 (replay-start-time game) 1500.0 350.0 0.06)))
                            (hash-set! players loser
                                       (player 1500.0 350.0 0.06
                                               '()
                                               '()
                                               '()
                                               '())))
                          (let* ([old-winner (hash-ref players winner)]
                                 [old-loser (hash-ref players loser)]
                                 [new-winner (struct-copy player old-winner
                                                          [op-name (cons loser (player-op-name old-winner))]
                                                          [op-r (cons (player-r old-loser) (player-op-r old-winner))]
                                                          [op-rd (cons (player-rd old-loser) (player-op-rd old-winner))]
                                                          [outcomes (cons 1 (player-outcomes old-winner))])]
                                 [new-loser (struct-copy player old-loser
                                                         [op-name (cons winner (player-op-name old-loser))]
                                                         [op-r (cons (player-r old-winner) (player-op-r old-loser))]
                                                         [op-rd (cons (player-rd old-winner) (player-op-r old-loser))]
                                                         [outcomes (cons 0 (player-outcomes old-loser))])])
                            (hash-set! player-history winner
                                       (cons
                                        (list 2
                                              (replay-start-time game)
                                              'Won
                                              (if (string=? spy winner) 'spy 'sniper)
                                              loser
                                              (player-r old-loser)
                                              (player-rd old-loser)
                                              (player-σ old-loser))
                                        (hash-ref player-history winner)))
                            (hash-set! player-history loser
                                       (cons
                                        (list 2
                                              (replay-start-time game)
                                              'Lost
                                              (if (string=? spy loser) 'spy 'sniper)
                                              winner
                                              (player-r old-winner)
                                              (player-rd old-winner)
                                              (player-σ old-winner))
                                        (hash-ref player-history loser)))
                            (hash-set! players winner new-winner)
                            (hash-set! players loser new-loser))
                          (stream-rest v))))))
        ; Show in a list-box.
        (define temp-frame (new frame% [label "Results"] [parent main-frame]
                                [width 750] [height 750]))
        (send temp-frame show #t)
        (send temp-frame center 'both)
        (define l-box
          (new list-box%
               [parent temp-frame]
               [label #f]
               [choices '()]
               [columns '("Player" "Rating")]
               [style (list 'single 'column-headers 'variable-columns 'clickable-headers)]
               [callback
                (lambda (list-box control-event)
                  (let* ([event-type (send control-event get-event-type)]
                         ; Wraparound when to avoid errors on single click
                         [c (if (equal? event-type 'list-box)
                                null
                                (if (equal? event-type 'list-box-dclick)
                                    (send list-box get-selections)
                                    (send control-event get-column)))])
                    (match (list event-type c)
                      [(list 'list-box-column 0)
                       (if (vector-ref modal11 0)
                           (let ([sorted (sort (hash->list players) string>? #:key car)])
                             (vector-set! modal11 0 #f)
                             (set! current-shown sorted)
                             (send l-box set
                                   (foldl (lambda (x y) (cons (car x) y)) '() sorted)
                                   (foldl (lambda (x y) (cons (~a (~r (player-r (cdr x)) #:precision 0)
                                                                  " ± "
                                                                  (~r (player-rd (cdr x)) #:precision 0))
                                                              y)) '() sorted)))
                           (let ([sorted (sort (hash->list players) string<? #:key car)])
                             (vector-set! modal11 0 #t)
                             (set! current-shown sorted)
                             (send l-box set
                                   (foldl (lambda (x y) (cons (car x) y)) '() sorted)
                                   (foldl (lambda (x y) (cons (~a (~r (player-r (cdr x)) #:precision 0)
                                                                  " ± "
                                                                  (~r (player-rd (cdr x)) #:precision 0))
                                                              y)) '() sorted))))]
                      [(list 'list-box-column 1)
                       (if (vector-ref modal12 0)
                           (let ([sorted (sort (hash->list players) < #:key (lambda (x) (player-r (cdr x))))])
                             (vector-set! modal12 0 #f)
                             (set! current-shown sorted)
                             (send l-box set
                                   (foldl (lambda (x y) (cons (car x) y)) '() sorted)
                                   (foldl (lambda (x y) (cons (~a (~r (player-r (cdr x)) #:precision 0)
                                                                  " ± "
                                                                  (~r (player-rd (cdr x)) #:precision 0))
                                                              y)) '() sorted)))
                           (let ([sorted (sort (hash->list players) > #:key (lambda (x) (player-r (cdr x))))])
                             (vector-set! modal12 0 #t)
                             (set! current-shown sorted)
                             (send l-box set
                                   (foldl (lambda (x y) (cons (car x) y)) '() sorted)
                                   (foldl (lambda (x y) (cons (~a (~r (player-r (cdr x)) #:precision 0)
                                                                  " ± "
                                                                  (~r (player-rd (cdr x)) #:precision 0))
                                                              y)) '() sorted))))]
                      [(list 'list-box-dclick _)
                       (begin
                         (define really-temp-frame (new frame%
                                                        [label "Player History"]
                                                        [parent temp-frame]
                                                        [min-width 750]
                                                        [min-height 750]))
                         (define plot-menu (new menu-bar% [parent really-temp-frame]))
                         (define plot-menu1 (new menu% [parent plot-menu] [label "Plot"]))
                         (define plot-menu-item
                           (new menu-item% [parent plot-menu1]
                                [label "Plot History"]
                                [callback
                                 (lambda (x y)
                                   (plot-new-window? #t)
                                   (plot (list (error-bars (for/list ([i (filter (lambda (x) (= 0 (car x)))
                                                                                 (reverse (hash-ref player-history
                                                                                                    (car (list-ref current-shown
                                                                                                                   (- (length current-shown)
                                                                                                                      (car (send list-box get-selections))
                                                                                                                      1))))))])
                                                             (list (list-ref i 1) (list-ref i 2) (list-ref i 3))))
                                               (points (for/list ([i (filter (lambda (x) (= 0 (car x)))
                                                                             (reverse (hash-ref player-history
                                                                                                (car (list-ref current-shown
                                                                                                               (- (length current-shown)
                                                                                                                  (car (send list-box get-selections))
                                                                                                                  1))))))])
                                                         (list (list-ref i 1) (list-ref i 2)))))
                                         #:x-label "Date"
                                         #:y-label "Rating"))]))
                         (define text (new text% [auto-wrap #t]))
                         (send text hide-caret #t)
                         (define viewer (new editor-canvas% [parent really-temp-frame] [editor text]))
                         (send really-temp-frame show #t)
                         (send really-temp-frame center 'both)
                         (send text insert
                               (apply ~a (cons
                                          (~a "User '" (car (list-ref current-shown
                                                                      (- (length current-shown)
                                                                         (car (send list-box get-selections))
                                                                         1)))
                                              "'\n")
                                          (cons
                                           (let loop ([t (filter (lambda (x) (= 2 (car x)))
                                                                 (reverse (hash-ref player-history
                                                                                    (car (list-ref current-shown
                                                                                                   (- (length current-shown)
                                                                                                      (car (send list-box get-selections))
                                                                                                      1))))))]
                                                      [spy-wins 0]
                                                      [sniper-wins 0]
                                                      [spy-games 0]
                                                      [sniper-games 0]
                                                      [total-games 0])
                                             (if (empty? t)
                                                 (~a "Won "
                                                     (+ spy-wins sniper-wins)
                                                     " out of "
                                                     total-games
                                                     " games ("
                                                     (~r (/ (+ spy-wins sniper-wins) total-games) #:precision 2)
                                                     "%)\n"
                                                     "Spy: "
                                                     spy-wins
                                                     " out of "
                                                     spy-games
                                                     " ("
                                                     (~r (/ spy-wins spy-games) #:precision 2)
                                                     "%)\n"
                                                     "Sniper: "
                                                     sniper-wins
                                                     " out of "
                                                     sniper-games
                                                     " ("
                                                     (~r (/ sniper-wins sniper-games) #:precision 2)
                                                     "%)\r\n")
                                                 (loop (rest t)
                                                       (if (and (equal? 'spy (cadddr (first t)))
                                                                (equal? 'Won (caddr (first t))))
                                                           (+ 1 spy-wins)
                                                           spy-wins)
                                                       (if (and (equal? 'sniper (cadddr (first t)))
                                                                (equal? 'Won (caddr (first t))))
                                                           (+ 1 sniper-wins)
                                                           sniper-wins)
                                                       (if (equal? 'spy (cadddr (first t))) (+ 1 spy-games) spy-games)
                                                       (if (equal? 'sniper (cadddr (first t))) (+ 1 sniper-games) sniper-games)
                                                       (+ 1 total-games))))
                                           (for/list ([e (in-list (reverse (hash-ref player-history
                                                                                     (car (list-ref current-shown
                                                                                                    (- (length current-shown)
                                                                                                       (car (send list-box get-selections)) 1))))))])
                                             (match (first e)
                                               [0 (~a "Updated "
                                                      (date->string (seconds->date (cadr e)) #t)
                                                      " ("
                                                      (caddr e)
                                                      " "
                                                      (cadddr e)
                                                      " "
                                                      (car (cddddr e))
                                                      ")\n")]
                                               [1 (~a "Created " (date->string (seconds->date (cadr e)) #t) " ("
                                                      (caddr e)
                                                      " "
                                                      (cadddr e)
                                                      " "
                                                      (car (cddddr e))
                                                      ")\n")]
                                               [2 (~a (caddr e)
                                                      " as "
                                                      (cadddr e)
                                                      " versus "
                                                      (list-ref e 4)
                                                      " ("
                                                      (list-ref e 5)
                                                      " "
                                                      (list-ref e 6)
                                                      " "
                                                      (list-ref e 7)
                                                      ")\n")])))))))]
                      [(list _ _) null])))]))
        (send l-box set-column-width 0 350 0 10000)
        (send l-box set-column-width 1 350 0 10000)
        (define current-shown null)
        (let ([sorted (hash->list players)])
          (set! current-shown sorted)
          (send l-box set
                (foldl (lambda (x y) (cons (car x) y)) '() sorted)
                (foldl (lambda (x y) (cons (~a (~r (player-r (cdr x)) #:precision 0)
                                               " ± "
                                               (~r (player-rd (cdr x)) #:precision 0))
                                           y)) '() sorted)))))))

(define run-stats (new menu-item% [parent stats]
                       [label "Get Stats"]
                       [shortcut-prefix (list 'ctl)]
                       [shortcut #\S]
                       [callback stats-callback]))

(define modal11 (vector #t))
(define modal12 (vector #t))

(define filter-menu (new menu% [label "Filter"] [parent menu-bar]))
(define filter-submenu (new menu-item% [label "Filter"] [parent filter-menu]
                            [shortcut-prefix (list 'ctl)]
                            [shortcut #\F]
                            [callback
                             (lambda (parent menu-bar)
                               (define filter-menu (new dialog% [label "Filter"] [alignment (list 'center 'center)]))
                               (define layout-frame (new horizontal-panel% [parent filter-menu]))
                               (define filter-p1 (new text-field% (parent layout-frame) (label "Username ")))
                               (define filter-role (new choice% (parent layout-frame)
                                                        (choices (list "Spy or Sniper"
                                                                       "Spy"
                                                                       "Sniper"))
                                                        (label " as  ")))
                               (define layout2 (new panel% (parent filter-menu)))
                               (define filter-p2 (new text-field% (parent layout-frame) (label " versus  ")))
                               (define modal #t)
                               (define all-venues (new check-box% (parent layout-frame) (label "on all venues ")
                                                       (value #t)
                                                       (callback (lambda (check-box control-event)
                                                                   (if modal
                                                                       (begin
                                                                         (set! modal (not modal))
                                                                         (send all-venues set-value #f)
                                                                         (send venue-list enable #t)
                                                                         (send layout2 add-child venue-list))
                                                                       (begin
                                                                         (set! modal (not modal))
                                                                         (send all-venues set-value #t)
                                                                         (send venue-list enable #f)
                                                                         (send layout2 delete-child venue-list)))))))
                               (define venue-list (new list-box% (parent layout2) (label "Select Venues:")
                                                       (enabled #f)
                                                       (min-height 375)
                                                       (style (list 'multiple 'deleted))
                                                       (choices (list "Veranda"
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
                                                                      "Modern"))))
                               (define filter-time1 (new text-field%
                                                         [label " between  "]
                                                         [parent layout-frame]
                                                         [init-value (date->string (seconds->date 0) #t)]))
                               (define filter-time2 (new text-field%
                                                         [label " and  "]
                                                         [parent layout-frame]
                                                         [init-value (date->string (current-date) #t)]))
                               (define filter-go
                                 (new button%
                                      (parent layout-frame)
                                      (label "Filter")
                                      (callback
                                       (λ (button control-element)
                                         (begin-busy-cursor)
                                         (let* ([p1-name (send filter-p1 get-value)]
                                                [role (send filter-role get-string-selection)]
                                                [p2-name (send filter-p2 get-value)]
                                                [venue (if (send all-venues get-value)
                                                           #f
                                                           (for/list ([i (in-list (send venue-list get-selections))])
                                                             (list-ref (list "Veranda"
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
                                                                        "Modern")
                                                                       i)))]
                                                [parse-date (lambda (s)
                                                              (match (date-display-format)
                                                                ['iso-8601 (string->date s
                                                                                         "~Y-~m-~dT~H:~M:~S")]
                                                                ['rfc2822 (string->date s
                                                                                        "~a, ~d ~b ~Y ~H:~M:~S ~z")]))]
                                                [start-time (with-handlers
                                                                ([exn:fail? (lambda (e) (begin
                                                                                          (message-box "Error!"
                                                                                                       "Bad date string in the start-time slot. If you're filling out a custom date, make sure to exactly follow the chosen date format.")
                                                                                          (seconds->date 0)))])
                                                              (parse-date (send filter-time1 get-value)))]
                                                [end-time (with-handlers
                                                              ([exn:fail? (lambda (e) (begin
                                                                                        (message-box "Error!"
                                                                                                     "Bad date string in the end-time slot. If you're filling out a custom date, make sure to exactly follow the chosen date format.")
                                                                                        (current-date)))])
                                                            (parse-date (send filter-time2 get-value)))]
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
                                                                    (set-member? venue (replay-venue a)))
                                                                (>= (replay-start-time a) (date->seconds start-time))
                                                                (<= (replay-start-time a) (date->seconds end-time))))])
                                           (let* ([temp-sorted (vector-filter temp-filter (send replay-select get-replays))]
                                                  [temp-frame (new frame% [label "Filter Results"] [min-width 1000] [min-height 900])]
                                                  [temp-replays (new replay-list-box%
                                                                     [parent temp-frame]
                                                                     [label #f]
                                                                     [choices '()]
                                                                     [callback replay-select-callback]
                                                                     [style (list 'extended 'column-headers 'clickable-headers 'reorderable-headers)]
                                                                     [min-height 500]
                                                                     [columns (list "Spy"
                                                                                    "Sniper"
                                                                                    "Outcome"
                                                                                    "Venue"
                                                                                    "Date")])])
                                             (send temp-replays set-column-width 0 200 0 10000)
                                             (send temp-replays set-column-width 1 200 0 10000)
                                             (send temp-replays set-column-width 2 150 0 10000)
                                             (send temp-replays set-column-width 3 175 0 10000)
                                             (send temp-replays set-column-width 4 200 0 10000)
                                             (send temp-replays set-replays! temp-sorted)
                                             (send filter-menu show #f)
                                             (send temp-frame show #t))
                                           (end-busy-cursor))))))
                               (send filter-menu show #t))]))

(define date-menu (new menu% (label "Date") (parent menu-bar)))

(define (helper-callback s)
  (lambda (item event) (date-display-format s)
     (send replay-select update-replays!)))

(define iso (new menu-item% (label "ISO-8601") (parent date-menu) (callback (helper-callback 'iso-8601))))
(define rfc (new menu-item% (label "RFC-2822") (parent date-menu) (callback (helper-callback 'rfc2822))))

; Default column widths for presentation
(send replay-select set-column-width 0 200 0 10000)
(send replay-select set-column-width 1 200 0 10000)
(send replay-select set-column-width 2 150 0 10000)
(send replay-select set-column-width 3 175 0 10000)
(send replay-select set-column-width 4 200 0 10000)