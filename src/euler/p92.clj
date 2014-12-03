(ns euler.p92
  (:require [euler.fns :as f]))


(defn square-digit
  "Returns sum of the squares of every digit in n."
  [n]
  (->> n f/digits (map #(* % %)) (reduce +)))


(defn terminator
  "Returns terminator (1 or 89) of square digit chain for n. A lookup map
  of known termination values can be provided to run faster, i.e. {1 1, 89, 89}.
  Passing an empty lookup will hang function."
  [n lookup]
  (if-let [t (get lookup n)]
	t
	(recur (square-digit n) lookup)
	))


; terminator lookup table for performance
(def term-lookup (atom {1 1, 89 89}))


(defn populate-lookup!
  "Populate lookup table for n up to 1000. (square-digit n) always comes in
  under 1000 when n < 1e7."
  []
  (doseq [n (range 1 1000)]
	(swap! term-lookup #(assoc % n (terminator n @term-lookup)))
	))


(defn p92
  []
  (populate-lookup!)
  (->>
   (range 1 1e7)
   (map #(terminator % @term-lookup))
   (filter #(= 89 %))
   count
   ))

