(ns euler.p32
	(:require [euler.fns :as f]))

(defn p32 []
	(reduce +
	(filter pos?
	(for [n (range 8000)]	; TODO smart way to find upper limit
		(if
			(->> (map #(vector % (/ n %) n) (f/proper-divisors n))
				(map #(= (range 1 10) (sort (mapcat f/digits %))))
				(some true?) boolean) n 0
				)))))
