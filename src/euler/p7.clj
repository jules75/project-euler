(ns euler.p7
	(:require [euler.fns :as f]
			[euler.prime :as p]))

(defn p7 []
	(nth (p/primes) 10000))
