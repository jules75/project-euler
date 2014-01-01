(ns euler.p30
	(:require [euler.fns :as f]
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
