(ns euler.p100
  (:require
   [clojure.math.numeric-tower :refer [expt sqrt]]
   ))


;
; Brute forcing the first 10 a/b pairs where a/b*(a-1)/(b-1) = 1/2 gives:
;
; 3			4
; 15		21
; 85		120
; 493		697
; 2871		4060
; 16731		23661
; 97513		137904
; 568345	803761
; 3312555	4684660
; 19306983	27304197
;
; From this we can see that:
;
; 1) The a/b ratio converges to sqrt(0.5)
; 2) For higher values, each b is approx 5.828427125 larger than the previous b
; 3) We can use 2) to estimate where the next b value will lie
; 4) We can use 1) to get the corresponding a value
;



(def SQRT-HALF (sqrt 0.5))


(defn calc-range
  "For all b in range m->n, calculate a/b pairs that satisfy formula
  a(a-1)/b(b-1) = 1/2."
  [m n]
  (for [b (range m n)
		:let [a (inc (long (*' b SQRT-HALF)))]
		:when (= (*' 2 a (dec a)) (*' b (dec b)))]
	[a b]))


(defn append-next-pair
  "Given ordered a/b pairs, estimate then isolate next a/b pair.
  Returns pairs with new pair appended."
  [pairs]
  (let [b-ratio (->> pairs (map last) (take-last 2) reverse (apply /) (* 1.0))
		next-b-approx (long (* b-ratio (last (last pairs))))
		search-dist 1000]
	(concat pairs (calc-range (- next-b-approx search-dist) (+ next-b-approx search-dist)))
	))


(defn solve
  "Given ordered a/b pairs, keep appending new pairs until the
  new pair exceeds the limit. New pair is included in result."
  [limit pairs]
  (if (< limit (last (last pairs)))
	pairs
	(recur limit (append-next-pair pairs))
	))


(defn p100
  []
  (->> (calc-range 2 1000)
	   (solve 1e12)
	   last
	   first))

