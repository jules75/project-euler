(ns euler.p56
	(:require [euler.fns :as f]
			[clojure.math.numeric-tower :as math]))
	
(defn p56 []
	(->> (for [a (range 100) b (range 100)] (math/expt a b))
		(map f/digits)
		(map #(apply + %))
		(reduce max)))
