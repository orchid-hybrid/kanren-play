#lang cKanren

(require cKanren/miniKanren)
(require cKanren/neq)        ;; =/=
(require cKanren/attributes) ;; symbol

(define (lesso n m)
  (fresh (ns ms)
    (conde
     ((== n 'z))
     ((== n `(s ,ns))
      (== m `(s ,ms))
      (lesso ns ms)))))

(define (contexto v t stage gamma)
  (conde
   ((fresh (_)
      (conso `(,v ,t ,stage) _ gamma)))
   ((fresh (head-v head-t tail)
      (conso `(,head-v . ,head-t) tail gamma)
      (=/= v head-v)
      (contexto v t stage tail)))))

(define (typeo stage gamma v t)
  (conde
   ((symbol v)
    (fresh (stage-prime)
      (contexto v t stage-prime gamma)
      (lesso stage-prime stage)))
   ((fresh (m n)
      (== `(,m ,n) v)
      (fresh (s)
        (typeo stage gamma m `(-> ,s ,t))
        (typeo stage gamma n s))))
   ((fresh (x m)
      (== v `(lam (,x) ,m))
      (symbol x)
      (fresh (a b)
        (== t `(-> ,a ,b))
        (typeo stage `((,x ,a ,stage) . ,gamma) m b))))
   
   ((fresh (m mt)
      (== `(brk ,m) v)
      (== `(brk ,mt) t)
      (typeo `(s ,stage) gamma m mt)))
   ((fresh (m ss)
      (== `(~ ,m) v)
      (== `(s ,ss) stage)
      (typeo ss gamma m `(brk ,t))))
   ))
