#lang plai
(require "./logication-library.rkt")
(require pict)

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

(define-type ExprXLevel
  [vals (img pict?) (level number?)])

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
    [(list 'NOT nexp) (l-not (parse nexp))]
    [else (error "Invalid syntax!")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERPRETATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; interp : Expr -> Image (pict)
;; Generates a circuit diagram from a given logical expression

(define (interp exp)
  (local [(define (interp exp level)
            (error "Call helper instead"))
          (define (helper exp level)
            (type-case Expr exp
              [id (name) (vals (text (symbol->string name)) level)]
              [bvalue (v) (vals (text (number->string v)) level)]
              ; use and-combine instead of and-combine2 if you want the original
              [l-and (lhs rhs)
                     (let* [(lv (helper lhs level))
                            (lv-image (vals-img lv))
                            (lv-level (vals-level lv))
                            (rv (helper rhs level))
                            (rv-image (vals-img rv))
                            (rv-level (vals-level rv))]
                     (vals (and-combine lv-image rv-image lv-level rv-level)
                           (max (+ rv-level 2) (+ lv-level 2))))]
              [l-or (lhs rhs)
                     (let* [(lv (helper lhs level))
                            (lv-image (vals-img lv))
                            (lv-level (vals-level lv))
                            (rv (helper rhs level))
                            (rv-image (vals-img rv))
                            (rv-level (vals-level rv))]
                     (vals (or-combine lv-image rv-image lv-level rv-level)
                           (max (+ rv-level 2) (+ lv-level 2))))]
              [l-not (exp)
                     (local [(define nv (helper exp level))
                             (define nv-image (vals-img nv))
                             (define nv-level (vals-level nv))]
                     (vals (not-combine nv-image) (+ nv-level 2)))]
              [else (error "")]))]
    (hc-append (vals-img (helper exp 1)) output)))


; needs to work on testing, can't use the procedure test to compare images 
;(test (interp (parse 'a)) (text (symbol->string 'a)))
;(test (interp (parse 1)) (text (number->string 1)));
;(test (interp (parse '(a AND b))) (text "test"))
(interp (parse '(a AND (b AND c))))
(interp (parse '((a AND b) AND c)))
(interp (parse '((a AND b) AND (c AND d))))
(interp (parse '(a AND (b AND (c AND d)))))
(interp (parse '(((a AND b) AND c) AND d)))
(interp (parse '(((a AND b) AND (c AND d)) AND ((e AND f) AND (g AND h)))))

(interp (parse '(((a OR b) OR c) OR d)))
(interp (parse '(NOT a)))
(interp (parse '((NOT a) AND b)))