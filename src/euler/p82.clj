(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(require '[taoensso.timbre.profiling :as profiling
		   :refer (pspy pspy* profile defnp p p*)])


(def INF 9999999999)


(defn neighbours
  "Returns neighbour coords of row/col"
  [height width [row col]]
  (let [bounded? (fn [h w [r c]] (and (< -1 r h) (< -1 c w)))
		coords #{[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]}]
	(filter #(bounded? height width %) coords)))


(def neighbours+ (memoize neighbours))


(defn value
  [tree [row col]]
  (get-in tree [:rows row col]))


(defn cost
  [tree [row col]]
  (get-in tree [:costs row col]))


(defn unvisited-neighbours
  [height width visited coord]
  (p :unvisited-neighbours
	 (difference (set (neighbours+ height width coord)) visited)
	 ))


(defn candidate-nodes
  "Find all visited nodes and their unvisited neighbours.
  Returns map of node/neighbours entries."
  [tree]
  (into {}
		(let [visited (:visited tree)
			  rows (:rows tree)
			  height (count rows)
			  width (count (first rows))
			  f (partial unvisited-neighbours height width visited)
			  nmap (map (juxt identity f) visited)]
		  (remove #(empty? (last %)) nmap)
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


(defn mark-visited
  "Return tree with nodes (as coords) marked as visited."
  [tree nodes]
  (if (seq nodes)
	(recur
	 (update-in tree [:visited] #(conj % (first nodes)))
	 (rest nodes))
	tree))


(defn complete?
  "True if tree if all nodes visited."
  [tree]
  (= (count (:visited tree)) (count (flatten (:rows tree)))))


(defn process-one
  [tree]
  (let [f (partial cost tree)
		g (partial value tree)
		candidates (candidate-nodes tree) 							; visited nodes with unvisited neighbours
		node (first (sort-by #(f (key %)) candidates)) 				; cheapeast node (TODO: use reducer for better performance)
		neighbs (val node)
		new-scores 	(map #(min (f %) (+ (f (key node)) (g %))) neighbs)
		]
	(-> tree
		(update-costs (zipmap neighbs new-scores))
		(mark-visited neighbs)
		)))


(defn process
  [tree]
  (if (complete? tree)
	tree
	(recur (process-one tree))
	))


(defn matrix->tree
  "Returns cost tree representation of matrix, assumes top
  left cell as origin."
  [matrix]
  (let [h (count matrix)
		w (count (first matrix))]
	(->
	 {:rows (vec (map vec matrix))
	  :visited #{[0 0]}
	  :costs (vec (repeat h (vec (repeat w INF))))}
	 (assoc-in [:costs 0 0] (-> matrix first first))
	 )))


#_(def tree35
	(->>
	 "https://projecteuler.net/project/resources/p082_matrix.txt"
	 slurp
	 (re-seq #"\d+")
	 (map #(Integer/parseInt %))
	 (take (* 35 35))
	 (partition 35)
	 matrix->tree
	 ))


#_(def tree5
  [[131 673 234 103 18]
   [201 96 342 965 150]
   [630 803 746 422 111]
   [537 699 497 121 956]
   [805 732 524 37 331]])


;(-> tree5 matrix->tree process-one)


;(defn main [] (process tree35))

;(profile :info :Arithmetic (main))

; 176071

