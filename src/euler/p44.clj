(ns euler.p44
	(:require [euler.fns :as f]))
		
(defn p44 []
	(first
	(let [pents (take 10000 (rest (f/pentagonals)))]
		(for [a pents b pents
			:when (< a b)
			:when (f/pentagonal? (- b a))
			:when (f/pentagonal? (+ a b))]
			(- b a)))))
