(ns euler.p83
  (:require [euler.p82 :refer [matrix->tree dijkstra-process]]))


(def neighbours
  ; "Returns neighbour coords of row/col"
  (memoize
   (fn
	 [height width [row col]]
	 (let [bounded? (fn [h w [r c]] (and (< -1 r h) (< -1 c w)))
		   coords #{[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]}]
	   (filter #(bounded? height width %) coords)
	   ))))


(defn solve
  [data]
  (->> data
	   (matrix->tree [0 0])
	   (dijkstra-process neighbours)
	   :costs
	   last
	   last
	   ))


(defn p83
  []
  (->>
   "https://projecteuler.net/project/resources/p083_matrix.txt"
   slurp
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (partition 80)
   solve
   ))
