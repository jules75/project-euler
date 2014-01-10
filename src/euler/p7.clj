(ns euler.p7
	(:require [euler.fns :as f]))

(defn p7 []
	(nth (f/primes) 10000))
