(ns euler.p27
	(:require [euler.fns :as f]))

(defn p27 []
	(->> (for [a (range -1000 1000) b (range -1000 1000)] 
		(vector a b (count (f/quadratic-primes a b))))
		(filter #(< 60 (last %)))
		(sort-by last)
		last 
		butlast 
		(apply *)))
