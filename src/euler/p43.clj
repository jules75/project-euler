(ns euler.p43
	(:require [euler.fns :as f]
		[clojure.set :as s]))

;		
; Brute force was too slow for this problem. It's better to calculate all
; possible multiples of 17 (the last triplet), and work back from there.
; This yields much more inelegant code, but runs MUCH faster.
;
; TODO refactor the whole thing - perhaps use recursion?
;

(defn unique-digits?
	[n]
	"True if n has no repeated digits"
	(let [d (f/digits n)]
		(= (count d) (count (distinct d)))))
		
(defn init 
	[]
	"All 3 digit multiples of 17 with unique digits"
	(->> (rest (map #(* 17 %) (range)))
		(take-while #(< % 1000))
		(filter unique-digits?)
		(map vector)))

(defn prepend-multiple
	[n m]
	"Returns all multiple of m that;
		- are 3 (or fewer) digit numbers 
		- contain unique digits 
		- have last 2 digits that are the first 2 digits of 3-digit n"
	(->> (map #(+ % (int (/ n 10))) (range 0 1000 100))
		(filter unique-digits?)
		(filter #(zero? (rem % m)))
		))

(defn prepend-missing
	[n]
	"Given 9-digit n that contains only unique digits, prepend the 
	missing digit, e.g. 106357289 => 4106357289"
	(f/undigits
		(let [digits (f/digits n)]
			(cons (first (s/difference (set (range 10)) (set digits))) digits)
			)))

(defn assemble
	[coll]
	"Re-assemble numbers, e.g. 231 319 195 952 => 231952"
	(f/undigits
		(let [digits (map f/digits coll)]
			(concat (first digits) (map last (rest digits)))
			)))

(defn prepend 
	[n coll]
	(->> coll
		(remove #(empty? (prepend-multiple (first %) n)))
		(mapcat #(for [n (prepend-multiple (first %) n)] (cons n %)))
		(filter #(unique-digits? (assemble %)))
		))

(defn p43 []
	(->> (init) 
		(prepend 13)
		(prepend 11)
		(prepend 7)
		(prepend 5)
		(prepend 3)
		(prepend 2)
		(map assemble)
		(remove #(< (count (f/digits %)) 9))
		(map prepend-missing)
		(reduce +)
		))
