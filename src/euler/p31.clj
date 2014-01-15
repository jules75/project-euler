(ns euler.p31
	(:require [euler.fns :as f]))
	
(defn p31 []
	(->>
		(for [n100 (range (inc 2))
			n50 (range (inc 4))
			n20 (range (inc 10))
			n10 (range (inc 20))
			n5 (range (inc 40))
			n2 (range (inc 100))	; no need to calc 1c, they make up the difference
			:let [big (+ (* 100 n100) (* 50 n50) (* 20 n20))]
			:when (>= 200 big)	; catch big overruns
			:when (>= 200 (+ big (* 10 n10) (* 5 n5) (* 2 n2)))
			]
			nil)
		count
		inc))			; include single 1 x 200c instance
