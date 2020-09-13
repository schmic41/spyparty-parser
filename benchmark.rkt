#lang racket

(require "helpers.rkt")

(for ([x (in-list (range 100))])
      (time (get-games "D:\\Documents\\all scl replays")))