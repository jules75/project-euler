(ns euler.p43
	(:require [euler.fns :as f]
		[clojure.set :as s]))

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

; ugly as hell, but MUCH faster than brute force
; TODO refactor
(defn p43 []
	(->> 
		(init) 
		
		(remove #(empty? (prepend-multiple (first %) 13)))
		(mapcat #(for [n (prepend-multiple (first %) 13)] (cons n %)))
		(filter #(unique-digits? (assemble %)))
		
		(remove #(empty? (prepend-multiple (first %) 11)))
		(mapcat #(for [n (prepend-multiple (first %) 11)] (cons n %)))
		(filter #(unique-digits? (assemble %)))
		
		(remove #(empty? (prepend-multiple (first %) 7)))
		(mapcat #(for [n (prepend-multiple (first %) 7)] (cons n %)))
		(filter #(unique-digits? (assemble %)))
		
		(remove #(empty? (prepend-multiple (first %) 5)))
		(mapcat #(for [n (prepend-multiple (first %) 5)] (cons n %)))
		(filter #(unique-digits? (assemble %)))
		
		(remove #(empty? (prepend-multiple (first %) 3)))
		(mapcat #(for [n (prepend-multiple (first %) 3)] (cons n %)))
		(filter #(unique-digits? (assemble %)))		
		
		(remove #(empty? (prepend-multiple (first %) 2)))
		(mapcat #(for [n (prepend-multiple (first %) 2)] (cons n %)))
		(filter #(unique-digits? (assemble %)))		

		(map assemble)
		(remove #(< (count (f/digits %)) 9))
		(map prepend-missing)
		(reduce +)
		
		))
