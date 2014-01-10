(ns euler.p6
	(:require [euler.fns :as f]))

(defn p6 []
	(let [r (range 101) 
		a (f/sq (apply + r))
		b (apply + (map f/sq r))]
		(bigint (- a b))))
