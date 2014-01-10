(ns euler.p50
	(:require [euler.fns :as f]
		;[clojure.contrib.profile :as p]
		[clojure.pprint :as pp]))

;
; Testing relationship between limit, series length, high prime and sum:
;
; limit	len	high	sum
; 100	6	13		41
; 1000	21	89		953
; 2000	27	131		1823
; 5000	45	227		4871
; 7200	55	263		6599
; 9800	65	317		9521
; 12800	73	379		12713
; 20000	81	463		19013
; 24200	100	541		24133
; 28800	108	593		28697
; 33800	115	641		33623
;
; There appears to be a clear relationships between the values:
;  - len is < sqrt(limit/2)
;  - high is about 5-6 times len (but multiplier is growing)
;  - sum approaching 99% of limit
;
; There are 78498 primes under a million, hopefully we can find the solution
; by testing the top 1% (785 primes)
;

(defn pair>
	[a b]
	"Reducer fn to sort by largest first value"
	(cond
		(> (first a) (first b)) a
		(and (= (first a) (first b)) (> (last a) (last b))) a
		:else b))

(defn longest-consec-prime-sum
	[n primes]
	"Returns sum of longest series of consecutive prime numbers
	E.g. given 1000, returns [21 953], where 21 is number of primes in the
	longest series, and 953 is their sum
	Primes <= n are passed in for performance"		
	(->>
		(for [r (range n 0 -1)] 
			(->> (drop r primes)				; get consecutive primes
			(reductions +)						; add them together
			(take-while #(< % n))				; discard large values
			(zipmap (range 1 n))				; attach number of terms
			(filter #(f/prime? (val %)))))		; discard non-primes
		(remove empty?)
		(map #(reduce pair> %))
		(reduce pair>)
		))

(defn p50 []
	(time (doall
	(let [n 100000
		primes (take-while #(< % n) (f/primes))]
		(longest-consec-prime-sum n primes)
		))))
