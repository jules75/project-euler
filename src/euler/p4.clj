(ns euler.p4
	(:require [euler.fns :as f]))

(defn p4 []
	(->> (for [a (range 1 1000) b (range 1 1000)] (* a b))
		(filter f/palindrome?)
		(apply max)))
