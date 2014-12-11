(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(def INF 9999999999)


(defn bounded?
  "True if row/col is a valid coord for tree."
  [tree [row col]]
  (and
   (< -1 row (count (:rows tree)))
   (< -1 col (count (first (:rows tree))))))


(defn unvisited?
  "True if row/col is not visited."
  [tree [row col]]
  (not (some #{[row col]} (:visited tree))))


(defn neighbours
  "Returns neighbour coords of row/col"
  [tree [row col]]
  (let [coords #{[(dec row) col] [(inc row) col] [row (dec col)] [row (inc col)]}]
	(filter (partial bounded? tree) coords)))


(defn value
  [tree [row col]]
  (get-in tree [:rows row col]))


(defn cost
  [tree [row col]]
  (get-in tree [:costs row col]))


(defn candidate-nodes
  "Find all visited nodes with unvisited neighbours.
  Returns map with node/neighbours entries."
  [tree]
  (into {}
		(let [visited (:visited tree)
			  neighbs (map (partial neighbours tree) visited)
			  nmap (zipmap visited (map #(filter (partial unvisited? tree) %) neighbs))]
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


(def tree1 {
			:rows [[131 673 234]
				   [201 96 342]
				   [630 803 746]]

			:visited #{[0 0]}

			:costs [[131 INF INF]
					[INF INF INF]
					[INF INF INF]]

			})


(process tree1)

