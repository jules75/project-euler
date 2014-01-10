(ns euler.p10 
	(:require [euler.fns :as f]))
	
(defn p10 [] ; TODO very slow
	(apply + (take-while #(< % 2000000) (f/primes))))
