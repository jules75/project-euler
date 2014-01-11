(ns euler.p9
	(:require [euler.fns :as f]))
	
(defn triplets
	[n]
	"Return all 3 number combinations whose sum is n"
	(for [a (range (inc n))
		b (range (- (inc n) a))]
		[a b (- n a b)]))
		
(defn p9 []
	(first
	(for [[a b c] (triplets 1000)
		:when (f/pythagorean? a b c)]
		(* a b c))))
