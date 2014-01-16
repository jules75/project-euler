(ns euler.p50
	(:require [euler.fns :as f]
		[euler.prime :as p]
		[clojure.pprint :as pp]))

(def primes-to-1m (p/primes 1000000))

(defn prime-sums
	[start max]
	"Returns sums of all sequences of consecutive primes
	2 100 => 2 5 10 17 28 41 58 77
	19 100 => 19 42 71 102 139 180"
	(->> primes-to-1m
		(drop-while #(< % start))
		(reductions +)
		(take-while #(< % max))))

(defn prime-sums-max
	[start max]
	"Calculate longest sequence of consecutive primes not exceeding max
	whose sum is prime. Returns [length sum], [0 0] if none found."
	(let [s (prime-sums start max)]
		(if (seq s)
			(->>
				(zipmap (-> s count inc range rest) s)		; attach counts to sums
				(filter #(p/prime? (last %)))				; keep only primes
				(reduce #(if (> (key %1) (key %2)) %1 %2))	; find longest series
				) [0 0])))

(defn p50 []
	(let [limit 1000000]
		(->> (range (Math/sqrt (/ limit 2)))				; analysis shows len<sqrt(lim/2)
			(map #(prime-sums-max % limit))
			(reduce #(if (> (first %) (first %2)) % %2))	; find longest series
			last)))
