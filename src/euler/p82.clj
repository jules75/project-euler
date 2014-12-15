(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(require '[taoensso.timbre.profiling :refer (profile p)])


(def neighbours
  ; "Returns neighbour coords of row/col"
  (memoize
   (fn
	 [height width [row col]]
	 (p :neighbours
		(let [bounded? (fn [h w [r c]] (and (< -1 r h) (< -1 c w)))
			  coords #{
					   [(dec row) col]
					   [(inc row) col]
					   ;[row (dec col)]
					   [row (inc col)]
					   }]
		  (filter #(bounded? height width %) coords)
		  )))))


(defn unvisited-neighbours
  "Returns nodes that are unvisited neighbours of supplied node."
  [tree node]
  (p :unvisited-neighbours
	 (let [height (count (:rows tree))
		   width (count (first (:rows tree)))]
	   (intersection (set (neighbours height width node)) (:unvisited tree))
	   )))


(defn update-costs
  "Returns tree with costs updated.
  Example score-map -> {[0 1] 804, [1 0] 332}"
  [tree score-map]
  (if (seq score-map)
	(let [[[row col] cost] (first score-map)]
	  (recur
	   (assoc-in tree [:costs row col] cost)
	   (rest score-map)))
	tree))


(defn cheapest
  "Returns node with lowest cost from given nodes."
  [tree nodes]
  (p :cheapest
	 (let [cost-map (zipmap nodes (map #(get-in (:costs tree) %) nodes))
		   f #(if (< (val %1) (val %2)) %1 %2)
		   best (first (reduce f cost-map))]
	   (first (filter #(= best %) nodes))
	   )))


(defn process-one
  [tree]
  (p :process-one
	 (let [get-value (fn [tree [row col]] (get-in tree [:rows row col]))
		   get-cost (fn [tree [row col]] (get-in tree [:costs row col]))
		   node (cheapest tree (:to-visit tree))
		   neighbs (unvisited-neighbours tree node)
		   f #(min (get-cost tree %) (+ (get-cost tree node) (get-value tree %)))]
	   (-> tree
		   (update-costs (zipmap neighbs (map f neighbs)))
		   (update-in [:visited] #(union % #{node}))
		   (update-in [:to-visit] #(difference (union % neighbs) #{node}))
		   (update-in [:unvisited] #(difference % (union #{node} neighbs)))
		   ))))


(defn process
  [tree]
  (if (empty? (:unvisited tree))
	tree
	(recur (process-one tree))
	))


(defn matrix->tree
  "Returns cost tree representation of matrix, assumes top
  left cell as origin."
  [origin matrix]
  (let [h (count matrix)
		w (count (first matrix))
		vrows (vec (map vec matrix))
		[r c] origin
		all-nodes (set (for [a (range h) b (range w)] [a b]))]
	(->
	 {:rows vrows
	  :to-visit #{origin}
	  :visited #{}
	  :costs (vec (repeat h (vec (repeat w Double/POSITIVE_INFINITY))))}
	 (assoc-in [:costs r c] (get-in vrows [r c]))
	 (assoc :unvisited (difference all-nodes #{origin}))
	 )))


(def data80
  (->>
   "https://projecteuler.net/project/resources/p082_matrix.txt"
   slurp
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (partition 80)
   ))


(def data2
  [[131 673 234 103 18]
   [201 96 342 965 150]
   [630 803 746 422 111]
   [537 699 497 121 956]
   [805 732 524 37 331]])


(defn solve
  [data]
  (apply min
		 (for [row (range (count data))]
		   (->> data
				(matrix->tree [row 0])
				process
				:costs
				(map last)
				(apply min)
				)
		   )))


(profile :info :Arithmetic (time (solve data80)))

