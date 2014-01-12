(ns euler.p34
	(:require [euler.fns :as f]))

(defn p34 []
	(letfn [(sum-of-digit-factorials? [n]
		(= n (->> n f/digits (map f/factorial) (reduce +))))]
		(->> (range 3 100000)	; TODO smart way to find upper limit
			(filter sum-of-digit-factorials?)
			(reduce +))))
