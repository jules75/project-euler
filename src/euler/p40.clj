(ns euler.p40
	(:require [euler.fns :as f]))

(defn p40 []
	(let [ch (apply str (range 200000))
		d (map #(reduce *' (repeat % 10)) (range 7))]
		(->>(map #(->> % (nth ch) str Integer/parseInt) d)
			f/undigits f/digits (apply *))))
