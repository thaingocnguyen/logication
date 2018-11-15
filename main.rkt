#lang plai
(require "./logication-library.rkt")

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
  [l-not (nexp Expr?)])

(define *reserved-symbols* '(id AND OR NOT XOR NAND NOR XNOR))

;; valid-identifer? : any -> boolean
;; Determines whether the parameter is valid as an identifier name
;; (rejects any names that has been used in define-type)
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; valid-boolean? : number -> number
;; Determines whether the parameter is a valid b-value
;; (only accepts the numbers 0 and 1)
(define (valid-boolean? bool)
  (and (number? bool)
       (or (= bool 0) (= bool 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (parse sexp)
  (match sexp
    [(? valid-identifier?) (id sexp)]
    [(? valid-boolean?) (bvalue sexp)]
    [(list lhs 'AND rhs) (l-and (parse lhs) (parse rhs))]
    [(list lhs 'OR rhs) (l-or (parse lhs) (parse rhs))]
    [(list lhs 'XOR rhs) (l-xor (parse lhs) (parse rhs))]
    [(list lhs 'NAND rhs) (l-nand (parse lhs) (parse rhs))]
    [(list lhs 'NOR rhs) (l-nor (parse lhs) (parse rhs))]
    [(list lhs 'XNOR rhs) (l-xnor (parse lhs) (parse rhs))]
    [(list 'NOT nexp) (l-not nexp)]
    [else (error "Invalid syntax!")]))