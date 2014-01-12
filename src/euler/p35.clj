(ns euler.p35
	(:require [euler.fns :as f]))

(defn p35 [] 
	(letfn [(rotations [s] "All rotations of string, e.g. abc => bca => cab"
		(take (count s) (iterate #(str (apply str (rest %)) (first %)) s)))]
	(count (filter
		#(every? (fn [x] (f/prime? (Integer/parseInt x))) (rotations (str %)))
		(range 1000000)))))
