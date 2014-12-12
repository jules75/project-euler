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
  [height width visited to-visit coord]
  (p :unvisited-neighbours
	 (difference (set (neighbours+ height width coord)) visited to-visit)
	 ))


(defn candidate-nodes
  "Find all visited nodes and their unvisited neighbours.
  Returns map of node/neighbours entries."
  [tree]
  (into {}
		(let [visited (:visited tree)
			  to-visit (:to-visit tree)
			  rows (:rows tree)
			  height (count rows)
			  width (count (first rows))
			  f (partial unvisited-neighbours height width visited to-visit)
			  nmap (map (juxt identity f) to-visit)]
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


(defn mark-to-visit
  "Return tree with nodes (as coords) marked as to-visit."
  [tree nodes]
  (if (seq nodes)
	(recur
	 (update-in tree [:to-visit] #(conj % (first nodes)))
	 (rest nodes))
	tree))


(defn remove-to-visit
  "Remove node from to-visit list."
  [tree node]
  (update-in tree [:to-visit] #(difference % (set [node]))))


(defn complete?
  "True if tree if all nodes visited."
  [tree]
  (not-any? #{INF} (flatten (:costs tree))))


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
		(mark-visited [(key node)])
		(remove-to-visit (key node))
		(mark-to-visit neighbs)
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
	  :to-visit #{[0 0]}
	  :visited #{}
	  :costs (vec (repeat h (vec (repeat w INF))))}
	 (assoc-in [:costs 0 0] (-> matrix first first))
	 )))


(def tree35
	(->>
	 "https://projecteuler.net/project/resources/p082_matrix.txt"
	 slurp
	 (re-seq #"\d+")
	 (map #(Integer/parseInt %))
	 (take (* 35 35))
	 (partition 35)
	 matrix->tree
	 ))


(defn main [] (process tree35))

(profile :info :Arithmetic (main))

; 176071
