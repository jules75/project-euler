(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(require '[taoensso.timbre.profiling :as profiling
		   :refer (pspy pspy* profile defnp p p*)])


(def neighbours
  ; "Returns neighbour coords of row/col"
  (memoize
   (fn
	 [height width [row col]]
	 (p :neighbours
		(let [bounded? (fn [h w [r c]] (and (< -1 r h) (< -1 c w)))
			  coords #{[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]}]
		  (filter #(bounded? height width %) coords)
		  )))))


(defn get-cost
  [tree [row col]]
  (p :get-cost
	 (get-in tree [:costs row col])))


(defn unvisited-neighbours
  [tree node]
  (p :unvisited-neighbours
	 (let [height (count (:rows tree))
		   width (count (first (:rows tree)))]
	   (intersection (set (neighbours height width node)) (:unvisited tree))
	   )))


(defn candidate-nodes
  "Find all visited nodes and their unvisited neighbours.
  Returns map of node/neighbours entries."
  [tree]
  (p :candidate-nodes
	 (into {}
		   (let [f #(unvisited-neighbours tree %)]
			 (map (juxt identity f) (:to-visit tree))
			 ))))


(defn update-costs
  "Returns tree with costs updated.
  Example score-map -> {[0 1] 804, [1 0] 332}"
  [tree score-map]
  (if (seq score-map)
	(let [[[row col] cost] (first score-map)]
	  (recur
	   (p :update-costs-recur (assoc-in tree [:costs row col] cost))
	   (rest score-map)))
	tree))


(defn mark-visited
  "Return tree with nodes (as coords) marked as visited."
  [tree nodes]
  (if (seq nodes)
	(recur
	 (p :mark-visited-recur (update-in tree [:visited] #(conj % (first nodes))))
	 (rest nodes))
	tree))


(defn mark-to-visit
  "Return tree with nodes (as coords) marked as to-visit."
  [tree nodes]
  (if (seq nodes)
	(recur
	 (p :mark-to-visit-recur (update-in tree [:to-visit] #(conj % (first nodes))))
	 (rest nodes))
	tree))


(defn remove-to-visit
  "Remove node from to-visit list."
  [tree node]
  (p :remove-to-visit
	 (update-in tree [:to-visit] #(difference % (set [node])))))


(defn remove-unvisited
  [tree nodes]
  (p :remove-unvisited
	 (assoc tree :unvisited (difference (:unvisited tree) nodes))
	 ))


(defn cheapest
  [tree nodes]
  (p :cheapest-node
	 (let [cost-map (zipmap nodes (map #(get-in (:costs tree) %) nodes))
		   f #(if (< (val %1) (val %2)) %1 %2)
		   best (first (reduce f cost-map))]
	   (first (filter #(= best %) nodes))
	   )))


(defn process-one
  [tree]
  (p :process-one
	 (let [value (fn [tree [row col]] (get-in tree [:rows row col]))
		   f (partial get-cost tree)
		   candidates (:to-visit tree)
		   node (cheapest tree candidates)
		   neighbs (unvisited-neighbours tree node)
		   new-scores (map #(min (f %) (+ (f node) (value tree %))) neighbs)
		   ]
	   (-> tree
		   (update-costs (zipmap neighbs new-scores))
		   (mark-visited [node])
		   (remove-to-visit node)
		   (mark-to-visit neighbs)
		   (remove-unvisited (union #{node} neighbs))
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
  [matrix]
  (let [h (count matrix)
		w (count (first matrix))
		all-nodes (set (for [a (range h) b (range w)] [a b]))]
	(->
	 {:rows (vec (map vec matrix))
	  :to-visit #{[0 0]}
	  :visited #{}
	  :costs (vec (repeat h (vec (repeat w Double/POSITIVE_INFINITY))))}
	 (assoc-in [:costs 0 0] (-> matrix first first))
	 (assoc :unvisited (difference all-nodes #{[0 0]}))
	 )))


#_(def tree80
  (->>
   "https://projecteuler.net/project/resources/p082_matrix.txt"
   slurp
   (re-seq #"\d+")
   (map #(Integer/parseInt %))
   (partition 80)
   matrix->tree
   ))


(defn main [] (-> tree80 process :costs last last))

(profile :info :Arithmetic (main))

; 45 -> 225615
; 50 -> 254220
; 60 -> 300190
; 70 -> 368598
; 80 -> 425185
