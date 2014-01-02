(ns euler.p30
	(:require [euler.fns :as f]
		[clojure.set :as set]
		[clojure.math.combinatorics :as c]))

(defn p30 []
	(letfn [(sum5p [n] (reduce + (map #(reduce * (repeat 5 %)) (f/digits n))))]
		(->> (range 2 999999)	; TODO smart way to find upper limit
			(filter #(= % (sum5p %)))
			(reduce +))))

(defn p31 []	; TODO doesn't work
	;(let [coins [1 2 5 10 20]]
		(->>
			(for [e (range 10) d (range 20) c (range 40) b (range 100) a (range 200)
				:when (= 200 (+ (* a 1) (* b 2) (* c 5) (* d 10) (* e 20)))]
				[a b c d e])
			count
			time
			))

(defn p32 []
	(reduce +
	(filter pos?
	(for [n (range 8000)]	; TODO smart way to find upper limit
		(if
			(->> (map #(vector % (/ n %) n) (f/proper-divisors n))
				(map #(= (range 1 10) (sort (mapcat f/digits %))))
				(some true?) boolean) n 0
				)))))

(defn p33 []
	(letfn [
		(remove-first [n coll]
			(let [[a b] (map vec (split-with #(not= n %) coll))] (into a (rest b))))
		(shared-digits [n m]
			(set/intersection (set (f/digits n)) (set (f/digits m))))]
	(->>
		(for [a (range 1 100) b (range 1 100) :when (and (< a b) (pos? (rem a 10)))]
			(let [shared (first (shared-digits a b))
				[aa bb] (map #(->> % f/digits (remove-first shared) f/undigits) [a b])]
				(if (and (pos? bb) (not= a aa) (= (/ a b) (/ aa bb)))
					[a b])))
		(remove nil?)
		(map #(/ (first %) (last %)))
		(reduce *))))

(defn p34 []
	(letfn [(sum-of-digit-factorials? [n]
		(= n (->> n f/digits (map f/factorial) (reduce +))))]
		(->> (range 3 100000)	; TODO smart way to find upper limit
			(filter sum-of-digit-factorials?)
			(reduce +))))

(defn p35 [] 
	(letfn [(rotations [s] "All rotations of string, e.g. abc => bca => cab"
		(take (count s) (iterate #(str (apply str (rest %)) (first %)) s)))]
	(count (filter
		#(every? (fn [x] (f/prime? (Integer/parseInt x))) (rotations (str %)))
		(range 1000000)))))

(defn p36 []
	(letfn [(base2 [n] "Return n as binary (as string), limited to 2^20"
		(->> (range 20) (map #(bit-and n (bit-shift-left 1 %)))
		(map #(if (pos? %) 1 0)) reverse (drop-while zero?) (apply str)))]
	(->> (range 1000000)
		(filter #(and (f/palindrome? %) (f/palindrome? (base2 %))))
		(reduce +))))

(defn p37 []
	(letfn [(truncate [n] "e.g. 1234 => [1 12 123 1234 1234 234 34 4]"
		(let [d (f/digits n), f #((partial (juxt take take-last) (inc %)) d)]
			(map f/undigits (mapcat f (range (count d))))))]
	(->> (range 11 1000000 2)
		(filter #(every? f/prime? (truncate %)))
		(take 11) ; only 11 truncatable primes with 2+ digits exist
		(reduce +))))
		
(defn p38 []
;
; This one was interesting, as it was far more practical to logically discard
; possibilities than to brute force a solution.
; 
; Concatenated product = concat(1x 2x 3x 4x .... nx)
; Seeking highest concatenated product that is 1-9 pandigital
;
; Let's consider the possible digit groupings for n;
;
; 2:  0000 00000
; 3:  000 000 000
; 4:  00 00 00 000
; 5:  0 00 00 00 00
; 6:  0 0 0 00 00 00
; 7:  0 0 0 0 0 00 00
; 8:  0 0 0 0 0 0 0 00
; 9:  0 0 0 0 0 0 0 0 0
;
; The highest pandigital we know is x=9 n=5 => 918273645. Knowing this, we can discard
; n >= 6, as the second grouping would have to be two digits (18).
;
; We can discard n=3 for the same reason, because the lowest starting value (x=912)
; gives 1824 for the second term, violating the digit grouping.
;
; This leaves n=2,4,5. We can discard n=4, because we would have to start with 
; (91,182,...) which violates digit grouping. We can discard n=5 as well, as 
; it was given as the example and so presumably isn't the answer (it isn't, I
; checked).
;
; Hooray! n=2 is the only possibility left!
;
; What about x values? The lowest starting x must be 9213, to get past our known
; value. The highest x possible is 9876. This leaves us with only 9876-9213+1 = 
; 663 values to check. Piece of cake.
;
	(->> (for [x (range 9213 9877)] (+ (* x 100000) (* 2 x))) ; all possible solutions
		(filter #(= (range 1 10) (sort (f/digits %)))) ; keep if pandigital 1-9
		(apply max)))
