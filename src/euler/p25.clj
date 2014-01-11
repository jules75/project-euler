(ns euler.p25
	(:require [euler.fns :as f]))

(defn p25 []
	(->> (map vector (range) (f/fibonacci))
		(filter #(= 1000 (count (str (last %)))))
		first first))
