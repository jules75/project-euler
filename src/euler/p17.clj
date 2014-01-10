(ns euler.p17
	(:require [euler.fns :as f]))
	
(defn as-words
	[n]
	"Return integer as words, supports 1 to 1000 inclusive"
	(let [units [nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"]
		tens [nil "ten" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]
		teens [nil "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"]
		rdigits (reverse (f/digits n))
		n-thou (get units (nth rdigits 3 0))
		n-hund (get units (rem (nth rdigits 2 0) 10))
		n-tens (get tens (rem (nth rdigits 1 0) 10))
		n-unit (get units (rem (nth rdigits 0 0) 10))
		n-teen (get teens (if (< 10 (rem n 100) 20) (- (rem n 100) 10)) nil)]
		(-> (str
			(when n-thou (str n-thou " thousand "))
			(when n-hund (str n-hund " hundred "))
			(when (and n-hund (or n-tens n-unit)) "and ")
			(if n-teen n-teen (str n-tens " " n-unit)))
		clojure.string/trim
		(clojure.string/replace "  " " "))))	

(defn p17 []
	(->
		(->> (range 1 1001) (map as-words) (apply str))
		(clojure.string/replace " " "")
		count))
