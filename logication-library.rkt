#lang plai
(require pict)

; Added AND gate written by Ollie
(define and-gate (rotate (vc-append (inset/clip (circle 30) 0 1 0  -15)
                       (inset/clip (rectangle 30 15) 0 -1 0 0)) (/ (* 3 pi) 2)))

; Default output image
(define output (hc-append 3 (hline 30 1) (text "output")))

; Default circuit line 
(define cline (hline 30 1))

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
              (text "output"))))

; and-combine testing (look at the function above for the original)
(define (and-combine2 input1 level1 input2 level2)
  (hc-append 
   (vl-append 2
              (hc-append input1 (hline (* 30 level2) 1))
              (hc-append input2 (hline (* 30 level1) 1)))
              and-gate))
