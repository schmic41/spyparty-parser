#lang racket

(require math/base)

(provide glicko (struct-out player))

(define π pi)
(define e euler.0)
;(define τ 0.4)

(struct player (r rd σ op-name op-r op-rd outcomes) #:transparent)

(define (glicko player-struct [τ 0.8])
  (if (empty? (player-outcomes player-struct))
      (values (player-r player-struct)
              (sqrt (+ (expt (player-rd player-struct) 2)
                       (expt (player-σ player-struct) 2)))
              (player-σ player-struct))
      (let* ([p-r (player-r player-struct)]
             [p-rd (player-rd player-struct)]
             [p-σ (player-σ player-struct)]
             [o-r (player-op-r player-struct)]
             [o-rd (player-op-rd player-struct)]
             [o-o (player-outcomes player-struct)]
             [p-μ (gr->g2r p-r)]
             [p-Φ (grd->g2rd p-rd)]
             [o-μ (map gr->g2r o-r)]
             [o-Φ (map grd->g2rd o-rd)]
             [vee (v p-μ o-μ o-Φ)]
             [delta (Δ o-o p-μ o-μ o-Φ)]
             [new-σ (σ’ p-σ delta vee p-Φ τ)]
             [Φ* (sqrt (+ (expt p-Φ 2) (expt new-σ 2)))]
             [Φ’ (/ 1 (sqrt (+ (/ 1 (expt Φ* 2)) (/ 1 vee))))]
             [μ’ (+ p-μ (* (expt Φ’ 2) (foldl + 0 (map
                                                   (lambda (μⱼ Φⱼ sⱼ)
                                                     (* (g Φⱼ)
                                                        (- sⱼ (E p-μ μⱼ Φⱼ))))
                                                   o-μ
                                                   o-Φ
                                                   o-o))))])
        (values (+ 1500 (* 173.7178 μ’)) (* 173.7178 Φ’) new-σ))))

(define (gr->g2r r)
  (/ (- r 1500) 173.7178))

(define (grd->g2rd rd)
  (/ rd 173.7178))

(define (v μ μl Φl)
  (/ 1
     (foldl + 0
            (map
             (lambda (μⱼ Φⱼ)
               (* (expt (g Φⱼ) 2)
                  (E μ μⱼ Φⱼ)
                  (- 1 (E μ μⱼ Φⱼ))))
             μl
             Φl))))

(define (g Φ)
  (/ 1 (sqrt (+ 1 (/ (* 3 (expt Φ 2)) (expt π 2))))))

(define (E μ μⱼ Φⱼ)
  (/ 1 (+ 1 (exp (* -1 (g Φⱼ) (- μ μⱼ))))))

(define (Δ s μ μl Φl)
  (* (v μ μl Φl)
     (foldl + 0
            (map
             (lambda (μⱼ Φⱼ sⱼ)
               (* (g Φⱼ)
                  (- sⱼ (E μ μⱼ Φⱼ))))
             μl
             Φl
             s))))

(define (σ’ σ delta vee Φ τ)
  (let* ([a (log (expt σ 2))]
         [f (lambda (x)
              (- (/ (* (exp x) (- (expt delta 2) (expt Φ 2) vee (exp x))) (* 2 (expt (+ vee (expt Φ 2) (exp x)) 2))) (/ (- x a) (expt τ 2))))]
         [ε 0.000001])
    (let recur ([A a]
                [B (if (> (expt delta 2) (+ vee (expt Φ 2)))
                       (log (- (expt delta 2) (expt Φ 2) vee))
                       (let recur ([k 1])
                         (if (negative? (f (- a (* k τ))))
                             (recur (+ k 1))
                             (- a (* k τ)))))]
                [fA (f a)]
                [fB (f (if (> (expt delta 2) (+ vee (expt Φ 2)))
                           (log (- (expt delta 2) (expt Φ 2) vee))
                           (let recur ([k 1])
                             (if (negative? (f (- a (* k τ))))
                                 (recur (+ k 1))
                                 (- a (* k τ))))))])
      (if (> (abs (- B A)) ε)
          (let* ([C (+ A (/ (* fA (- A B)) (- fB fA)))]
                 [fC (f C)])
            (if (negative? (* fC fB))
                (recur B C fB fC)
                (recur A C (/ fA 2) fC)))
          (exp (/ A 2))))))
