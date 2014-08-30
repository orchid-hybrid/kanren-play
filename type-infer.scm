#lang cKanren

(require cKanren/miniKanren)
(require cKanren/neq)
(require cKanren/attributes)

(define (contexto v t gamma)
  (conde
   ((fresh (_)
      (conso `(,v . ,t) _ gamma)))
   ((fresh (head-v head-t tail)
      (conso `(,head-v . ,head-t) tail gamma)
      (=/= v head-v)
      (contexto v t tail)))))

(define (typeo gamma v t)
  (conde
   ((symbol v)
    (contexto v t gamma))
   ((fresh (m n)
      (== `(,m ,n) v)
      (fresh (s)
        (typeo gamma m `(-> ,s ,t))
        (typeo gamma n s))))
   ((fresh (x m)
      (== v `(lam (,x) ,m))
      (symbol x)
      (fresh (a b)
        (== t `(-> ,a ,b))
        (typeo `((,x . ,a) . ,gamma) m b))))))
