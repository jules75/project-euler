(ns euler.p63
  (:require
   [clojure.math.numeric-tower :refer [expt]]
   ))


(defn candidate?
  [x n]
  "True if x^n has n digits."
  (= n (count (str (expt x n)))))

(defn p63
  []
  (count
   (flatten
	(for [n (range 1 25)]
	  (->> (range 1 10)
		   (filter #(candidate? % n))
		   (map #(expt % n))
		   )))))
