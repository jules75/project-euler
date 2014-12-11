(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(def INF 9999999999)

;(def counter (atom 0))


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


(defn candidate-nodes
  "Find all visited nodes with unvisited neighbours.
  Returns map of node/neighbours entries."
  [tree]
  (into {}
		(let [visited (:visited tree)
			  height (count (:rows tree))
			  width (count (first (:rows tree)))
			  neighbs (map #(difference (set (neighbours+ height width %)) (:visited tree)) visited)
			  nmap (zipmap visited neighbs)]
		  (remove #(empty? (val %)) nmap)
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


#_(def tree20
	(->>
	 "https://projecteuler.net/project/resources/p082_matrix.txt"
	 slurp
	 (re-seq #"\d+")
	 (map #(Integer/parseInt %))
	 (take 400)
	 (partition 20)
	 matrix->tree
	 ))


(time (process tree20))

;@counter
; 108221
