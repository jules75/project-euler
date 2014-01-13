(ns euler.p47
	(:require [euler.fns :as f]))

(defn p47 []
	(->> (range)
		(map #(vector % (-> % f/factor frequencies count)))
		(partition 4 1)
		(filter #(= (repeat 4 4) (map last %)))
		first first first))
