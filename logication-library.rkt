#lang plai
(require pict)

; Added AND gate written by Ollie
(define and-gate (rotate (vc-append (inset/clip (circle 30) 0 1 0  -15)
                       (inset/clip (rectangle 30 15) 0 -1 0 0)) (/ (* 3 pi) 2)))

; Default output image
(define output (text "output"))

; Default circuit line 
(define cline (hline 30 5))

(define p1 (text "a"))
(define p2 (text "b"))
(define p3 (hc-append 5 p1 (hline 30 5)))
(define p4 (hc-append 5 p2 (hline 30 5)))
(define p5 (vc-append 5 p3 p4))

(define p6 (hc-append and-gate (hline 30 5)))
(define p7 (hc-append 4 p6 (text "output")))
(define combined (hc-append p5 p7))

;; and-combine : Image Image -> Image
;; Takes the image of two inputs and construct and and-circuit image
(define (and-combine input1 input2)
  (hc-append 
   (vc-append 5
              (hc-append 5 input1 cline)
              (hc-append 5 input2 cline))
   (hc-append 4
              (hc-append and-gate cline)
              output)))
