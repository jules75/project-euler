(ns euler.p92
  (:require [euler.fns :as f]))


(def term-lookup (atom {1 1, 89 89}))
(def LIMIT 1e7)


(defn square-digit
  "Returns sum of the squares of every digit in n."
  [n]
  (->> n f/digits (map #(* % %)) (reduce +)))


(defn terminator
  "Returns terminator (1 or 89) of square digit chain for n. A lookup map
  of known termination values can be provided to run faster, i.e. {1 1, 89, 89}.
  Passing an empty lookup will hang function."
  [n lookup]
  (let [m (square-digit n)
		t (get lookup m)]
	(if t t (recur m lookup))))


(defn populate-lookup!
  "Populate lookup table for n up to 1000. (square-digit n) always comes in
  under 1000 when n < 1e7."
  []
  (doseq [n (range 1 (-> LIMIT Math/log10 int inc (* 9 9)))]
	(swap! term-lookup #(assoc % n (terminator n @term-lookup)))))


(defn boundary-diff
  "The square-digit-sums fn generates values using the differences between each subsequent
  value. This gets hairy when a number ticks over from ending in a 9 to ending in a 0.
  This function returns the difference to apply to a number ending in 9."
  [n]
  (let [[_ a b] (re-find #"([0-8]{0,1})(9+)$" (str n))
		p (if (seq a) (Integer/parseInt a) 0)
		q (inc p)
		m (count b)]
	(* -1 (+ (* 81 m) (* p p) (* -1 q q)))))


(defn square-digit-sums
  "Returns an infinte sequence of all square digit sums, starting
  with 1. Takes advantage of pattern of differences between each
  value, much faster than individually calculating each one."
  []
  (let [tens (map #(-> % inc (* 10) dec boundary-diff) (range))]
	(reductions + (mapcat #(vector 1 3 5 7 9 11 13 15 17 %) tens))))


(defn p92
  []
  (populate-lookup!)
  (->> (square-digit-sums)
	   (take LIMIT)
	   (map #(get @term-lookup %))
	   (filter #(= 89 %))
	   count))


;(time (p92))
