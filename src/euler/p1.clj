(ns euler.p1
	(:require [euler.fns :as f]))

(defn p1 []
	(reduce + 
		(filter #(f/any-divisible? % [3 5]) (range 1000))))
