(ns euler.p27
	(:require [euler.fns :as f]
		[euler.prime :as p]))

(defn quadratic-primes
	[a b]
	"Starting with n=0, produce consecutive primes from quadratic nn + an + b"
	(take-while p/prime? (map #(+ (* % %) (* a %) b) (range))))
	
(def max-qpc (atom 0))	; highest quad prime count found
	
(defn p27 []
	(->> (for [a (range -1000 1000) 
				b (p/primes 1000) ; b must be prime - why?
				:let [c (count (quadratic-primes a b))]
				:when (> c @max-qpc)
				]
			(do
				(reset! max-qpc c)
				(vector [a b] c)
			))
		(reduce #(if (> (last %) (last %2)) % %2))
		first
		(apply *)
		))
