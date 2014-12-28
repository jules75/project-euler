(ns euler.p60
  (:require [euler.prime :as p]))


(def primes-to-10k (p/primes 10000))


(defn prime-pair?
  [pair]
  (let [append #(->> (Math/log10 %2) int inc (Math/pow 10) (* %1) (+ %2) int)]
	(p/prime-fast? (apply append pair) primes-to-10k)))


(defn p60
  []
  (first
   (for [a primes-to-10k
		 b primes-to-10k
		 :when (< a b)
		 :when (every? prime-pair? [[a b] [b a]])
		 c primes-to-10k
		 :when (< b c)
		 :when (every? prime-pair? [[a c] [b c] [c a] [c b]])
		 d primes-to-10k
		 :when (< c d)
		 :when (every? prime-pair? [[a d] [b d] [c d] [d a] [d b] [d c]])
		 e primes-to-10k
		 :when (< d e)
		 :when (every? prime-pair? [[a e] [b e] [c e] [d e] [e a] [e b] [e c] [e d]])]
	 (+ a b c d e)
	 )))

