#lang racket

; TODO: Add map and selected mission support

(require racket/match)

(provide (struct-out replay) (struct-out exception) parse-game)

;;;

(struct exception (type info))

(struct replay (filename
                file-version
                protocol-version
                game-version
                duration
                uuid
                start-time
                result
                game-type
                selected-missions
                picked-missions
                completed-missions
                spy-username
                spy-displayname
                sniper-username
                sniper-displayname
                venue
                venue-variant
                guest-count)
  #:transparent)

;;;

(define *venue-variants*
  (hash 
   "Teien" (list
            "BooksBooksBooks"
            "BooksStatuesBooks"
            "StatuesBooksBooks"
            "StatuesStatuesBooks"
            "BooksBooksStatues"
            "BooksStatuesStatues"
            "StatuesBooksStatues"
            "StatuesStatuesStatues")
   "Aquarium" (list
               "Bottom"
               "Top")))

(define (get-venue bytes)
  (match (integer-bytes->integer bytes #f)
    ; Why?
    [#x6f81a558 "Veranda"]
    [#x9dc5bb5e "Courtyard"]
    [#x168f4f62 "Library"]
    [#x1dbd8e41 "Balcony"]
    [#x7173b8bf "Gallery"]
    [#x9032ce22 "Terrace"]
    [#x2e37f15b "Moderne"]
    [#x79dfa0cf "Teien"]
    [#x98e45d99 "Aquarium"]
    [#x3a30c326 "BvB High-Rise"]
    [#x5996faaa "BvB Ballroom"]
    [#x5b121925 "Ballroom"]
    [#x1a56c5a1 "High-Rise"]
    [#x28b3aa5e "Old-Art Gallery"]
    [#x290a0c75 "Old-Art Courtyard 2"]
    [#x3695f583 "Old-Art Panopticon"]
    [#xa8bea091 "Old-Art Veranda"]
    [#xb8891fbc "Old-Art Balcony"]
    [#xd027340 "Crowded Pub"]
    [#x3b85fff3 "Pub"]
    [#x9c2e7b0 "Old-Art Ballroom"]
    [#xb4cf686b "Old-Art Courtyard 1"]
    [#x7076e38f "Double Modern"]
    [#xe6146120 "Modern"]
    [4091941985 "Modern"]
    [#x35ac5135 "Redwoods"]
    ; If map's not found, just punt it to an integer, then a string
    [_ (~a (integer-bytes->integer bytes #f))]))

;;;

(define (v3-parse filename file-version header)
  (if (equal? (subbytes header 0 4) #"RPLY")
      (let* ([protocol-version (integer-bytes->integer (subbytes header 8 12) #f)]
             [game-version (integer-bytes->integer (subbytes header 12 16) #f)]
             [game-duration (floating-point-bytes->real (subbytes header 20 24))]
             [uuid (subbytes header 24 40)]
             [start (integer-bytes->integer (subbytes header 40 44) #f)]
             [seqid (subbytes header 44 46)]
             [spy-length (integer-bytes->integer (subbytes header 46 47) #f)]
             [sniper-length (integer-bytes->integer (subbytes header 47 48) #f)]
             [outcome (match (subbytes header #x30 #x34)
                        [#"\0\0\0\0" "Missions Win"]
                        [#"\1\0\0\0" "Time Out"]
                        [#"\2\0\0\0" "Spy Shot"]
                        [#"\3\0\0\0" "Civilian Shot"]
                        [#"\4\0\0\0" "In-Progress"]
                        [#"\5\0\0\0" "Number of Results"]
                        [_ (error 'outcome filename)])]
             [base-type (match (arithmetic-shift (integer-bytes->integer (subbytes header #x34 #x38) #f) -28)   
                          [0 "k"]
                          [1 "p"]
                          [2 "a"]
                          [_ (error 'base-type filename)])]
             [required (bitwise-and (integer-bytes->integer (subbytes header #x34 #x38) #f) #x00003FFF)]
             [available
                        (arithmetic-shift (bitwise-and (integer-bytes->integer (subbytes header #x34 #x38) #f) #x0FFFC000) -14)]
             [selected-missions (let* ([tempint (integer-bytes->integer (subbytes header #x3C #x40) #f)]
                                       [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                       [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                       [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                       [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                       [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                       [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                       [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                       [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                 (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [picked-missions (let* ([tempint (integer-bytes->integer (subbytes header #x40 #x44) #f)]
                                     [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                     [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                     [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                     [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                     [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                     [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                     [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                     [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [completed-missions (let* ([tempint (integer-bytes->integer (subbytes header #x44 #x48) #f)]
                                        [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                        [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                        [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                        [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                        [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                        [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                        [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                        [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                   (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [game-type (if (equal? base-type "Known")
                            (~a base-type available)
                            (~a base-type required "/" available))]
             [spy-name (bytes->string/utf-8 (subbytes header 80 (+ 80 spy-length)))]
             [sniper-name (bytes->string/utf-8 (subbytes header
                                                         (+ 80 spy-length)
                                                         (+ 80 spy-length sniper-length)))]
             [venue (get-venue (subbytes header #x38 (+ 4 #x38)))])
        (replay
         filename
         (integer-bytes->integer file-version #f)
         protocol-version
         game-version
         game-duration
         uuid
         start
         outcome
         game-type
         selected-missions
         picked-missions
         completed-missions
         spy-name
         #f
         sniper-name
         #f
         venue
         #f
         #f))
      (exception 'file-not-valid filename)))

(define (v4-parse filename file-version header)
  (if (equal? (subbytes header 0 4) #"RPLY")
      (let* ([protocol-version (integer-bytes->integer (subbytes header 8 12) #f)]
             [game-version (integer-bytes->integer (subbytes header 12 16) #f)]
             [game-duration (floating-point-bytes->real (subbytes header 20 24))]
             [uuid (subbytes header 24 40)]
             [start (integer-bytes->integer (subbytes header 40 44) #f)]
             [seqid (subbytes header 44 46)]
             [spy-length (integer-bytes->integer (subbytes header 46 47) #f)]
             [sniper-length (integer-bytes->integer (subbytes header 47 48) #f)]
             [outcome (match (subbytes header #x34 #x38)
                        [#"\0\0\0\0" "Missions Win"]
                        [#"\1\0\0\0" "Time Out"]
                        [#"\2\0\0\0" "Spy Shot"]
                        [#"\3\0\0\0" "Civilian Shot"]
                        [#"\4\0\0\0" "In-Progress"]
                        [#"\5\0\0\0" "Number of Results"]
                        [_ (error 'outcome filename)])]
             [base-type (match (arithmetic-shift (integer-bytes->integer (subbytes header #x38 60) #f) -28)
                          [0 "k"]
                          [1 "p"]
                          [2 "a"]
                          [_ (error 'base-type filename)])]
             [required (bitwise-and (integer-bytes->integer (subbytes header #x38 60) #f) #x00003FFF)]
             [available (arithmetic-shift (bitwise-and (integer-bytes->integer (subbytes header #x38 60) #f) #x0FFFC000) -14)]
             [selected-missions (let* ([tempint (integer-bytes->integer (subbytes header #x40 #x44) #f)]
                                       [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                       [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                       [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                       [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                       [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                       [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                       [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                       [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                 (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [picked-missions (let* ([tempint (integer-bytes->integer (subbytes header #x44 #x48) #f)]
                                     [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                     [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                     [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                     [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                     [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                     [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                     [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                     [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [completed-missions (let* ([tempint (integer-bytes->integer (subbytes header #x48 #x4C) #f)]
                                        [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                        [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                        [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                        [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                        [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                        [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                        [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                        [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                   (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [game-type (if (equal? base-type "Known")
                            (~a base-type available)
                            (~a base-type required "/" available))]
             [spy-name (bytes->string/utf-8 (subbytes header #x54 (+ #x54 spy-length)))]
             [sniper-name (bytes->string/utf-8 (subbytes header
                                                         (+ #x54 spy-length)
                                                         (+ #x54 spy-length sniper-length)))]
             [venue (get-venue (subbytes header #x3C (+ 4 #x3C)))])
        (replay
         filename
         (integer-bytes->integer file-version #f)
         protocol-version
         game-version
         game-duration
         uuid
         start
         outcome
         game-type
         selected-missions
         picked-missions
         completed-missions
         spy-name
         #f
         sniper-name
         #f
         venue
         #f
         #f))
      (exception 'file-not-valid filename)))

(define (v5-parse filename file-version header)
  (if (equal? (subbytes header 0 4) #"RPLY")
      (let* ([protocol-version (integer-bytes->integer (subbytes header #x8 #xC) #f)]
             [game-version (integer-bytes->integer (subbytes header #xC #x10) #f)]
             [game-duration (floating-point-bytes->real (subbytes header #x14 #x18))]
             [uuid (subbytes header #x18 #x28)]
             [start (integer-bytes->integer (subbytes header #x28 #x2C) #f)]
             [seqid (subbytes header #x2C #x2E)]
             [spy-username-length (integer-bytes->integer (subbytes header #x2E #x2F) #f)]
             [spy-displayname-length (integer-bytes->integer (subbytes header #x30 #x31) #f)]
             [sniper-username-length (integer-bytes->integer (subbytes header #x2F #x30) #f)]
             [sniper-displayname-length (integer-bytes->integer (subbytes header #x31 #x32) #f)]
             [spy-username (bytes->string/utf-8 (subbytes header #x60
                                                          (+ #x60 spy-username-length)))]
             [sniper-username (bytes->string/utf-8
                               (subbytes header
                                         (+ #x60 spy-username-length)
                                         (+ #x60 spy-username-length sniper-username-length)))]
             [spy-displayname (if (= 0 spy-displayname-length)
                                  spy-username
                                  (bytes->string/utf-8
                                   (subbytes header
                                             (+ #x60 spy-username-length sniper-username-length)
                                             (+ #x60
                                                spy-username-length
                                                sniper-username-length
                                                spy-displayname-length))))]
             [sniper-displayname (if (= 0 sniper-displayname-length)
                                     sniper-username
                                     (bytes->string/utf-8
                                      (subbytes header
                                                (+ #x60
                                                   spy-username-length
                                                   sniper-username-length
                                                   spy-displayname-length)
                                                (+ #x60
                                                   spy-username-length
                                                   sniper-username-length
                                                   spy-displayname-length
                                                   sniper-displayname-length))))]
             [outcome (match (subbytes header #x38 #x3C)
                        [#"\0\0\0\0" "Missions Win"]
                        [#"\1\0\0\0" "Time Out"]
                        [#"\2\0\0\0" "Spy Shot"]
                        [#"\3\0\0\0" "Civilian Shot"]
                        [#"\4\0\0\0" "In-Progress"]
                        [#"\5\0\0\0" "Number of Results"]
                        [_ (error 'outcome filename)])]
             [base-type (match (arithmetic-shift (integer-bytes->integer (subbytes header #x3C #x40) #f) -28)
                          [0 "k"]
                          [1 "p"]
                          [2 "a"]
                          [_ (error 'base-type filename)])]
             [required (bitwise-and (integer-bytes->integer (subbytes header #x3C #x40) #f) #x00003FFF)]
             [available (arithmetic-shift (bitwise-and (integer-bytes->integer (subbytes header #x3C #x40) #f) #x0FFFC000) -14)]
             [selected-missions (let* ([tempint (integer-bytes->integer (subbytes header #x48 #x4C) #f)]
                                       [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                       [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                       [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                       [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                       [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                       [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                       [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                       [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                 (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [picked-missions (let* ([tempint (integer-bytes->integer (subbytes header #x4C #x50) #f)]
                                     [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                     [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                     [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                     [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                     [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                     [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                     [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                     [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [completed-missions (let* ([tempint (integer-bytes->integer (subbytes header #x50 #x54) #f)]
                                        [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                        [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                        [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                        [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                        [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                        [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                        [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                        [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                   (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [game-type (if (equal? base-type "Known")
                            (~a base-type available)
                            (~a base-type required "/" available))]
             [venue (get-venue (subbytes header #x40 (+ 4 #x40)))]
             [guest-count (integer-bytes->integer (subbytes header #x50 #x54) #f)])
        (replay
         filename
         (integer-bytes->integer file-version #f)
         protocol-version
         game-version
         game-duration
         uuid
         start
         outcome
         game-type
         selected-missions
         picked-missions
         completed-missions
         spy-username
         spy-displayname
         sniper-username
         sniper-displayname
         venue
         #f
         guest-count))
      (exception 'file-not-valid filename)))

(define (v6-parse filename file-version header)
  (if (equal? (subbytes header #x0 #x4) #"RPLY")
      (let* ([g-b (lambda (start end) (subbytes header start end))]
             [protocol-version (integer-bytes->integer (g-b #x8 #xC) #f)]
             [game-version (integer-bytes->integer (g-b #xC #x10) #f)]
             [game-duration (floating-point-bytes->real (g-b #x14 #x18))]
             [uuid (g-b #x18 #x28)]
             [start (integer-bytes->integer (g-b #x28 #x2C) #f)]
             [seqid (g-b #x2C #x2E)]
             [spy-username-length (integer-bytes->integer (g-b #x2E #x2F) #f)]
             [spy-displayname-length (integer-bytes->integer (g-b #x30 #x31) #f)]
             [sniper-username-length (integer-bytes->integer (g-b #x2F #x30) #f)]
             [sniper-displayname-length (integer-bytes->integer (g-b #x31 #x32) #f)]
             [spy-username (bytes->string/utf-8 (g-b #x64 (+ #x64 spy-username-length)))]
             [sniper-username (bytes->string/utf-8
                               (g-b
                                (+ #x64 spy-username-length)
                                (+ #x64 spy-username-length sniper-username-length)))]
             [spy-displayname (if (= 0 spy-displayname-length)
                                  spy-username
                                  (bytes->string/utf-8
                                   (g-b
                                    (+ #x64 spy-username-length sniper-username-length)
                                    (+ #x64
                                       spy-username-length
                                       sniper-username-length
                                       spy-displayname-length))))]
             [sniper-displayname (if (= 0 sniper-displayname-length)
                                     sniper-username
                                     (bytes->string/utf-8
                                      (g-b
                                       (+ #x64
                                          spy-username-length
                                          sniper-username-length
                                          spy-displayname-length)
                                       (+ #x64
                                          spy-username-length
                                          sniper-username-length
                                          spy-displayname-length
                                          sniper-displayname-length))))]
             [outcome (match (g-b #x38 #x3C)
                        [#"\0\0\0\0" "Missions Win"]
                        [#"\1\0\0\0" "Time Out"]
                        [#"\2\0\0\0" "Spy Shot"]
                        [#"\3\0\0\0" "Civilian Shot"]
                        [#"\4\0\0\0" "In-Progress"]
                        [#"\5\0\0\0" "Number of Results"]
                        [_ (error 'outcome filename)])]
             [base-type (match (arithmetic-shift (integer-bytes->integer (subbytes header #x3C #x40) #f) -28)
                          [0 "k"]
                          [1 "p"]
                          [2 "a"]
                          [_ (error 'base-type filename)])]
             [required (bitwise-and (integer-bytes->integer (subbytes header #x3C #x40) #f) #x00003FFF)]
             [available (arithmetic-shift (bitwise-and (integer-bytes->integer (subbytes header #x3C #x40) #f) #x0FFFC000) -14)]
             [selected-missions (let* ([tempint (integer-bytes->integer (subbytes header #x48 #x4C) #f)]
                                       [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                       [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                       [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                       [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                       [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                       [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                       [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                       [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                 (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [picked-missions (let* ([tempint (integer-bytes->integer (subbytes header #x4C #x50) #f)]
                                     [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                     [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                     [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                     [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                     [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                     [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                     [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                     [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [completed-missions (let* ([tempint (integer-bytes->integer (subbytes header #x50 #x54) #f)]
                                        [bug? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 0)))) "Bug Ambassador")]
                                        [contact? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 1)))) "Contact Double Agent")]
                                        [transfer? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 2)))) "Transfer Microfilm")]
                                        [swap? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 3)))) "Swap Statue")]
                                        [inspect? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 4)))) "Inspect Statues")]
                                        [seduce? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 5)))) "Seduce Target")]
                                        [purloin? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 6)))) "Purloin Guest List")]
                                        [fingerprint? (when (not (= 0 (bitwise-and tempint (arithmetic-shift 1 7)))) "Fingerprint Ambassador")])
                                   (list bug? contact? transfer? swap? inspect? seduce? purloin? fingerprint?))]
             [guest-count (integer-bytes->integer (subbytes header #x54 #x58) #f)]
             [start-duration (integer-bytes->integer (subbytes header #x58 (+ 4 #x58)) #f)]
             [game-type (if (equal? base-type "Known")
                            (~a base-type available)
                            (~a base-type required "/" available))]
             [venue (get-venue (subbytes header #x40 (+ 4 #x40)))]
             [venue-variant (if (or (equal? venue "Aquarium") (equal? venue "Teien"))
                                (list-ref (hash-ref *venue-variants* venue) (integer-bytes->integer (subbytes header #x44 (+ 4 #x44)) #t))
                                #f)])
        (replay
         filename
         (integer-bytes->integer file-version #f)
         protocol-version
         game-version
         game-duration
         uuid
         start
         outcome
         game-type
         selected-missions
         picked-missions
         completed-missions
         spy-username
         spy-displayname
         sniper-username
         sniper-displayname
         venue
         venue-variant
         guest-count))
      (exception 'file-not-valid filename)))

(define (parse-game g)
  (match (subbytes (cdr g) 4 8)
    [#"\6\0\0\0" (v6-parse (path->string (car g))
                           (subbytes (cdr g) 4 8)
                           (cdr g))]
    [#"\5\0\0\0" (v5-parse (path->string (car g))
                           (subbytes (cdr g) 4 8)
                           (cdr g))]
    [#"\4\0\0\0" (v4-parse (path->string (car g))
                           (subbytes (cdr g) 4 8)
                           (cdr g))]
    [#"\3\0\0\0" (v3-parse (path->string (car g))
                           (subbytes (cdr g) 4 8)
                           (cdr g))]
    [_ (exception 'file-version (car g))]))
