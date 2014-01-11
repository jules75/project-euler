(ns euler.p26
	(:require [euler.fns :as f]))

(defn p26 []
; 
; To calculate the length of period 1/p where p is prime, check if it is
; evenly divisible into 9, then 99, then 999, etc. When you find one divisible 
; by p, the number of 9s is period length.
;
; See point 9 at http://mathworld.wolfram.com/DecimalExpansion.html
; 
	(let [nines (iterate #(+ 9 (*' 10 %)) 9)
		primes (drop 3 (take-while #(< % 1000) (f/primes)))]
	(->>
		(for [p primes] (-> (filter #(zero? (rem % p)) nines) first str count))
		(zipmap primes)
		(sort-by val)
		last first)))
