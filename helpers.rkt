#lang racket

(require threading)
(require "parser.rkt")
(provide (all-defined-out))

; Helper definitions

(define (read-replay cns)
  (let* ([f (open-input-file (cdr cns))]
         [d (read-bytes 416 f)])
    (close-input-port f)
    (cons (file-name-from-path (cdr cns)) d)))

(define (get-replay-paths directory)
  (fold-files
   (lambda (p t l)
     (if (and
          (not (= 0 (file-size p)))
          (equal? (path-get-extension p) #".replay")
          (equal? 'file t))
         (cons (cons (file-name-from-path p) p) l)
         l))
   '()
   directory))

(define (get-games dir)
  (let ([x (~> dir
               get-replay-paths
               (map read-replay _)
               (map parse-game _))])
    (let loop ([y (first x)]
               [z (rest x)]
               [good '()]
               [bad '()])
      (if (empty? z)
          (match y
            ;[(exception 'file-version _) (list good (cons (exception-info y) bad))]
            ;[(exception 'file-not-valid _) (list good (cons (exception-info y) bad))]
            [_ (values (cons y good) bad)])
          (match y
            ;[(exception 'file-version _) (loop (first z) (rest z) good (cons (exception-info y) bad))]
            ;[(exception 'file-not-valid _) (loop (first z) (rest z) good (cons (exception-info y) bad))]
            [_ (loop (first z) (rest z) (cons y good) bad)])))))

(define (get-spy-name game)
  (if (replay-spy-displayname game)
      (replay-spy-displayname game)
      (replay-spy-username game)))

(define (get-sniper-name game)
  (if (replay-sniper-displayname game)
      (replay-sniper-displayname game)
      (replay-sniper-username game)))

(define (get-variant game)
  (when (replay-venue-variant game)
    (replay-venue-variant game)))
