(ns euler.p53
	(:require [euler.fns :as f]))

(defn p53 []
	(count
	(for [n (range 1 101) 
		r (range 1 n)
		:when (> (f/count-combinations n r) 1000000)]
		[n r])))
