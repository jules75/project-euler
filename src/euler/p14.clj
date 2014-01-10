(ns euler.p10)

(defn p14 [] ; TODO takes 2 minutes
	(->>
	(range 1 1000000)
	(map #(vector % (count (f/collatz %))))
	(reduce #(if (> (last %1) (last %2)) %1 %2))
	first
	))
