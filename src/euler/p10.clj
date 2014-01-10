(ns euler.p10 
	(:require [euler.fns :as f]))
	
(defn sieve
	([coll n maxn]
	(if (> n maxn)
		coll
		(recur (remove #(and (not= % n) (zero? (mod % n))) coll) (inc n) maxn)
		))
	([coll]
		(sieve coll 2 (-> (last coll) Math/sqrt int)))
	)

(defn p10 [] ; TODO very slow
	(time (count (sieve (range 2 1000000))))
	)
	;(time (reduce + (take-while #(< % 2000000) (f/primes)))))
