(ns euler.prime 
	(:require [euler.fns :as f]))

(defn sieve
	[mults coll]
	"Remove multiples of each n in mult from coll except 1n"
	(let [n (first mults)]
		(if (seq mults)
			(recur (rest mults) (remove #(and (zero? (unchecked-remainder-int % n)) (not= n %)) coll))
			coll)))

(defn prime?
	[n]
	"True if integer is prime"
	(if (> 2 n) false
		(= 1 (count (f/factor n)))))
	
(defn primes
	[]
	"Returns infinite sequence of primes"
	(cons 2 (filter prime? (iterate #(+ 2 %) 3))))
	