(ns euler.p71
  (:require [euler.fns :as f]))


(defn find-rpf
  "Given numerator n and denominator d, keep reducing n by one
  until a 'reducing proper fraction' is found."
  [n d]
  (if (= 1 (f/gcd n d))
	[n d]
	(recur (dec n) d)))


(defn max-ratio
  "Return maximum of a/b or c/d."
  [[a b] [c d]]
  (if (> (/ a b) (/ c d))
	[a b]
	[c d]))


(defn p71
  []
  (first
   (reduce max-ratio
		   (for [d (range 999000 1000000) 	; assume denom is near 1 million
				 :let [n (int (* d 3/7))]
				 :when (pos? (dec n))]
			 (find-rpf (dec n) d)
			 ))))

