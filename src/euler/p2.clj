(ns euler.p2
	(:require [euler.fns :as f]))

(defn p2 []
	(->> (take-while #(< % 4000000) (f/fibonacci))
		(filter even?)
		(apply +)))
