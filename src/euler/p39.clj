(ns euler.p39
	(:require [euler.fns :as f]))

(defn p39 []
	(->> (for [a (range 1 500) b (range 1 a) c (range 1 b)
			:when (even? (+ a b c))
			:when (> (+ c b) a)
			:when (f/pythagorean? c b a)] 
			(reduce + [a b c]))
		frequencies
		(sort-by last)
		last first))
