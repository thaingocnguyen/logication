#lang plai

;; Syntax specification:
;;
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

; Note: added "l-" to the name of functions because and, or, not, etc.
; exists in Racket
(define-type Expr
  [id (name symbol?)]
  [bvalue (bval number?)]
  [l-and (lhs Expr?) (rhs Expr?)]
  [l-or (lhs Expr?) (rhs Expr?)]
  [l-xor (lhs Expr?) (rhs Expr?)]
  [l-nand (lhs Expr?) (rhs Expr?)]
  [l-nor (lhs Expr?) (rhs Expr?)]
  [l-xnor (lhs Expr?) (rhs Expr?)]
  [l-not (exp Expr?)])

(define *reserved-symbols* '(id AND OR NOT XOR NAND NOR XNOR))

;; valid-identifer? : any -> boolean
;; Determines whether the parameter is valis as an identifier name
;; (rejects any names that has been used in define-type)
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
