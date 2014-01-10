(ns euler.p9
	(:require [euler.fns :as f]))
			
(defn p9 []	; TODO very slow
	(time (first
	(let [r (range 1 500)]
		(for [a r b r c r 
			:when (= 1000 (+ a b c))
			:when (f/pythagorean? a b c)]
			(* a b c)))
			)))
