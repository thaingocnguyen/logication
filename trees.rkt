#lang plai
(require pict)
(require pict/tree-layout)

;; EBNF
;; <Expr> ::= <id>
;;          | <B-value>
;;          | {<Expr> AND <Expr>}
;;          | {<Expr> OR <Expr>}
;;          | {NOT <Expr>}
;;          | {<Expr> XOR <Expr>}
;;          | {<Expr> NAND <Expr>}
;;          | {<Expr> NOR <Expr>}
;;          | {<Expr> XNOR <Expr>} 

;; <B-value> ::= 0
;;             | 1  




(define AND (vc-append (inset/clip (circle 30) 0 30 0  -15)
                       (inset/clip (rectangle 30 15) 0 -4 0 0)))


 (naive-layered (tree-layout
                  (tree-edge #:edge-width 3 (tree-layout))
                  (tree-edge #:edge-color "green" (tree-layout))))


(naive-layered (tree-layout #:pict (scale AND 2)
                            (tree-edge #:edge-width 2 (tree-layout #:pict AND))
                            (tree-edge #:edge-width 2 (tree-layout #:pict AND))))
