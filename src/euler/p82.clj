(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(def neighbours
  ; "Returns neighbour coords of row/col"
  (memoize
   (fn
	 [height width [row col]]
		(let [bounded? (fn [h w [r c]] (and (< -1 r h) (< -1 c w)))
			  coords #{[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]}]
		  (filter #(bounded? height width %) coords)
		  ))))


(defn unvisited-neighbours
  [tree node]
	 (let [height (count (:rows tree))
		   width (count (first (:rows tree)))]
	   (intersection (set (neighbours height width node)) (:unvisited tree))
	   ))


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
  [tree nodes]
	 (let [cost-map (zipmap nodes (map #(get-in (:costs tree) %) nodes))
		   f #(if (< (val %1) (val %2)) %1 %2)
		   best (first (reduce f cost-map))]
	   (first (filter #(= best %) nodes))
	   ))


(defn process-one
  [tree]
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
		   )))


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

(main)

; 45 -> 225615
; 50 -> 254220
; 60 -> 300190
; 70 -> 368598
; 80 -> 425185
