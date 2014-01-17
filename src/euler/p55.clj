(ns euler.p55
	(:require [euler.fns :as f]))
	
(defn palindrome
	[n]
	(->> n f/digits reverse f/undigits))
	
(defn lychrel?
	[n bailout]
	(->>
		(iterate #(+' % (palindrome %)) n)
		(take bailout)
		(filter #(= (str %) (apply str (reverse (str %)))))	; TODO BigInt f/palindrome?
		(filter #(not= n %))
		(take 1)
		empty?))

(defn p55 []
	(count (filter #(lychrel? % 50) (range 1 10000))))
