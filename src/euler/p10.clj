(ns euler.p10 
	(:require [euler.prime :as p]))

(defn p10 []
	(apply + (p/primes 2000000)))
