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

(define-type ExprXResult
  [vals (img pict?) (size number?)])

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
  (local [(define (interp exp)
            (error "Call helper instead"))
          (define (helper exp final input-size)
            (type-case Expr exp
              [id (name)
                  (local [(define img (text (symbol->string name)))
                          (define result (vl-append final img))
                          (define size (pict-width img))]
                    (vals result size))]
              [bvalue (v)
                      (local [(define img (text (number->string v)))
                              (define result (vl-append final img))
                              (define size (pict-width img))]
                        (vals result size))]
              [l-and (lhs rhs)
                     (local [(define lval (helper lhs final input-size))
                             (define lval-img (vals-img lval))
                             (define lval-size (vals-size lval))
                             (define rval (helper rhs lval-img lval-size))
                             (define rval-img (vals-img rval))
                             (define rval-size (vals-size rval))
                             (define result (hc-append final (hc-append (hc-append
                                                        (vl-append (hline (max (- (pict-width rval-img) rval-size)
                                                                               30)
                                                                          1)
                                                                   (hline (max (- (pict-width lval-img) lval-size)
                                                                               30)
                                                                          1))
                                                        and-gate)
                                                       (hline 30 1))))
                             (define result-size (max lval-size rval-size))]
                       (vals result result-size))]
              [else (error "")]))]
    (hc-append (vals-img (helper exp (blank) 0)) output)))


; needs to work on testing, can't use the procedure test to compare images 
;(test (interp (parse 'a)) (text (symbol->string 'a)))
;(test (interp (parse 1)) (text (number->string 1)));
;(test (interp (parse '(a AND b))) (text "test"))
;(interp (parse '(a AND (b AND c))))
;(interp (parse '((a AND b) AND c)))
;(interp (parse '((a AND b) AND (c AND d))))
;(interp (parse '(a AND (b AND (c AND d)))))
;(interp (parse '(((a AND b) AND c) AND d)))
;(interp (parse '(((a AND b) AND (c AND d)) AND ((e AND f) AND (g AND h)))))
;(interp (parse '(((a OR b) OR c) OR d)))
;(interp (parse '(NOT a)))
;(interp (parse '((NOT a) AND b)))

; ================ EXAMPLES =======================
(define (run exp)
  (interp (parse exp)))

(run 'a)
(run 1)
(run '(a AND b))
(run '((a AND b) AND c))
(run '(a AND (b AND c)))
