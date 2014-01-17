(ns euler.p62
	(:require [euler.fns :as f]))

(def cubes (map #(* % % %) (range 9000)))		; upper limit by trial and error

(def candidate-digits
	"Unordered digits that can be rearranged to form solution"
	(->> cubes							
		(map (comp frequencies f/digits))		; count digit freq in every cube
		frequencies								; count cubes with same digits
		(filter #(= 5 (val %)))					; find digits having 5 cubes
		first key								; reconstruct digits
		(map #(repeat (val %) (key %)))
		flatten))

(defn p62 []
	(->>
		(map f/digits cubes)
		(filter #(= (sort %) candidate-digits))
		(map f/undigits)
		(apply min)))
