(ns euler.p87
  (:require
   [euler.prime :as p]
   [clojure.math.numeric-tower :refer [expt]]
   ))


(defn solve
  [limit]
  (count
   (set
	(for [a (p/primes (expt limit (/ 1 2)))
		  b (p/primes (expt limit (/ 1 3)))
		  c (p/primes (expt limit (/ 1 4)))
		  :let [n (+ (expt a 2) (expt b 3) (expt c 4))]
		  :when (< n limit)]
	  n))))


(defn p87
  []
  (solve 50000000))

