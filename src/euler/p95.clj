(ns euler.p95
  (:require [euler.fns :refer [proper-divisors]])
  )


(defn make-map
  [n]
  (let [f #(apply + (proper-divisors %))]
	(into {}
		  (map (juxt identity f) (range 1 (inc n)))
		  )))


(def LIMIT 1000000)

(def nmap (make-map LIMIT))


(defn amicable?
  "Return true if first + last elements are same."
  [coll]
  (= (first coll) (last coll)))


(defn chain
  "Where coll is a numeric map, returns chain of numbers.
  Returns nil if any part of chain exceeds limits of map.
  Let ch be beginning vector stub, e.g. [12496]."
  [coll ch]
  (let [m (get coll (last ch))]
	(cond
	 (nil? m) 			nil
	 (some #{m} ch) 	(conj ch m)
	 :else 				(recur coll (conj ch m))
	 )))


(defn p95
  []
  (->> (range LIMIT)
	   (map #(chain nmap [%]))
	   (filter amicable?)
	   (reduce #(if (> (count %) (count %2)) % %2))
	   (apply min)
	   ))

