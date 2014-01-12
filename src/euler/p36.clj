(ns euler.p36
	(:require [euler.fns :as f]))

(defn p36 []
	(letfn [(base2 [n] "Return n as binary (as string), limited to 2^20"
		(->> (range 20) (map #(bit-and n (bit-shift-left 1 %)))
		(map #(if (pos? %) 1 0)) reverse (drop-while zero?) (apply str)))]
	(->> (range 1000000)
		(filter #(and (f/palindrome? %) (f/palindrome? (base2 %))))
		(reduce +))))
