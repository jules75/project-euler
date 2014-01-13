(ns euler.p43
	(:require [euler.fns :as f]
		[clojure.math.combinatorics :as c]))

(defn candidate? 
	[digits]
	(every? true?
		(map #(zero? (rem (f/undigits (subvec digits % (+ % 3))) %2))
			(range 1 8) (f/primes))))
				
(defn p43 []
	(->> (filter candidate? (c/permutations (range 10)))
		(map f/undigits)
		(apply +)))
