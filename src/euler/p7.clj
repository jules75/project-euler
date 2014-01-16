(ns euler.p7
	(:require [euler.prime :as p]))

(defn p7 []
	(nth (p/primes 2000000) 10000))
