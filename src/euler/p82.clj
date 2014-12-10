(ns euler.p82
  (:require [clojure.set :refer [union intersection difference]]))


(def data
  [[131 673 234 103 18]
   [201 96 342 965 150]
   [630 803 746 422 111]
   [537 699 497 121 956]
   [805 732 524 37 331]])


(defn neighbours
  [edges node]
  (->> edges
	   (filter #(some #{node} %))
	   flatten
	   (remove #{node})
	   set))


(defn add-score
  "Returns tree with node's score increased by n.
  Creates node score if needed."
  [tree node n]
  (let [m (get-in tree [:scores node])
		newval (if m (+ n m) (+ n (get-in tree [:nodes node])))]
	(assoc-in tree [:scores node] newval)
	))


(defn mark-visited
  "Returns tree with node marked as visited."
  [tree node]
  (update-in tree [:visited] #(conj % node)))


(defn unvisited-neighbours
  "Returns set of all unvisited neighbours in tree."
  [tree]
  (let [v (:visited tree)
		f #(neighbours (:edges tree) %)]
	(difference (apply union (map f v)) v)))


(defn visited?
  "True if all nodes in tree are visited."
  [tree]
  (= (count (:visited tree)) (count (:nodes tree))))


(defn dijkstra
  [tree]
  (if (visited? tree)
	tree
	(let [node (first (unvisited-neighbours tree))
		  neighbs (neighbours (:edges tree) node)
		  visited (intersection neighbs (:visited tree))
		  min-score (apply min (vals (select-keys (:scores tree) visited)))
		  _ (println tree)
		  _ (println node neighbs visited min-score)
		  ]
	  (recur (-> tree (add-score node min-score) (mark-visited node)))
	  )))



(def data
  {:nodes {:a 5 :b 7 :c 12 :d 9 :e 3}
   :edges #{[:a :b] [:c :a] [:b :c] [:b :d] [:c :e] [:d :e]}
   :visited #{:a :b}
   :scores {:a 5 :b 12}
   })


(dijkstra data)

