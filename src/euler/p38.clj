(ns euler.p38
	(:require [euler.fns :as f]))
		
(defn p38 []
;
; This one was interesting, as it was far more practical to logically discard
; possibilities than to brute force a solution.
; 
; Concatenated product = concat(1x 2x 3x 4x .... nx)
; Seeking highest concatenated product that is 1-9 pandigital
;
; Let's consider the possible digit groupings for n;
;
; 2:  0000 00000
; 3:  000 000 000
; 4:  00 00 00 000
; 5:  0 00 00 00 00
; 6:  0 0 0 00 00 00
; 7:  0 0 0 0 0 00 00
; 8:  0 0 0 0 0 0 0 00
; 9:  0 0 0 0 0 0 0 0 0
;
; The highest pandigital we know is x=9 n=5 => 918273645. Knowing this, we can discard
; n >= 6, as the second grouping would have to be two digits (18).
;
; We can discard n=3 for the same reason, because the lowest starting value (x=912)
; gives 1824 for the second term, violating the digit grouping.
;
; This leaves n=2,4,5. We can discard n=4, because we would have to start with 
; (91,182,...) which violates digit grouping. We can discard n=5 as well, as 
; it was given as the example and so presumably isn't the answer (it isn't, I
; checked).
;
; Hooray! n=2 is the only possibility left!
;
; What about x values? The lowest starting x must be 9213, to get past our known
; value. The highest x possible is 9876. This leaves us with only 9876-9213+1 = 
; 663 values to check. Piece of cake.
;
	(->> (for [x (range 9213 9877)] (+ (* x 100000) (* 2 x))) ; all possible solutions
		(filter #(= (range 1 10) (sort (f/digits %)))) ; keep if pandigital 1-9
		(apply max)))
