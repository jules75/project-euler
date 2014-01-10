(ns euler.p18
	(:require [euler.fns :as f]
			[clojure.math.combinatorics :as c]))
			
(defn walk-tritree
	[tritree path]
	"Walk path from top of triangle tree (see http://projecteuler.net/problem=18)
	Triangle tree is represented as single depth vector
	Path: 0=left 1=right, e.g. [0 0 0 1 1 0 1 0 1 0 1]
	Returns vector of values"
	(->> (let [r (range (count tritree))
		child-map (->> r								; number every node
			(map #(->> (f/untriangle %) int inc (+ %)))	; find each child index
			(map #(vector % (inc %)))					; and its neighbour
			(zipmap r))]								; build map
		(loop [p path visited [0]]
			(let [child (get-in child-map [(last visited) (first p)])]
				(if (seq p) (recur (rest p) (conj visited child)) visited)
				)))
				(map #(get tritree %))))

(defn p18 []
	(let [tri [
		75
		95 64
		17 47 82
		18 35 87 10
		20  4 82 47 65
		19  1 23 75  3 34
		88  2 77 73  7 63 67
		99 65  4 28  6 16 70 92
		41 41 26 56 83 40 80 70 33
		41 48 72 33 47 32 37 16 94 29
		53 71 44 65 25 43 91 52 97 51 14
		70 11 33 28 77 73 17 78 39 68 17 57
		91 71 52 38 17 14 91 43 58 50 27 29 48
		63 66  4 68 89 53 67 30 73 16 69 87 40 31
		04 62 98 27 23  9 70 98 73 93 38 53 60  4 23]]
		(->>
			(c/selections [0 1] 14)
			(map #(reduce + (walk-tritree tri %)))
			(apply max))))

