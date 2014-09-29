(ns euler.p67
	(:require [euler.fns :as f]
			[clojure.string :as s]))
(defn fetch
	[]
	(->> (slurp "https://projecteuler.net/project/resources/p067_triangle.txt")
		s/split-lines
		(map #(s/split % #"\s"))
		(map #(map (fn [s] (Integer/parseInt s)) %))
		(map vec)))

(defn process	; TODO apply this technique to problem 18
	[rows]
	"Recursively process the rows of the triangle, adding the highest
	value in each pair on the bottom row to its parent value in the row above
	Returns the last two rows"
	(if (< 2 (count rows))
		(let [maxline (map #(apply max %) (partition 2 1 (last rows)))
			newline (map + maxline (first (take-last 2 rows)))]
			(recur (conj (vec (drop-last 2 rows)) newline)))
		rows))

(defn p67 []
	(let [[r1 r2] (process (fetch))]
		(+ (first r1) (apply max r2))))
