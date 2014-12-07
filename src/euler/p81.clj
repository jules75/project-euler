(ns euler.p81)


(def data
  (->> "https://projecteuler.net/project/resources/p081_matrix.txt"
	   slurp
	   (re-seq #"\d+")
	   (map #(Integer/parseInt %))
	   (partition 80)))


(defn diamondify
  "Rotates two dimensional 'square' vector 45 degrees into a 'diamond'."
  [data]
  (let [stepped-data (map #(vec (concat (repeat %1 nil) %2)) (range) data)]
	(map #(remove nil? %)
		 (for [col (-> stepped-data first count (* 2) dec range)]
		   (map #(get % col) stepped-data)
		   ))))

(defn proc-row
  [[r1 r2]]
  (let [pairs (flatten (map #(repeat 2 %) r1))
		f #(map (partial apply min) %)]
	(map + r2
		 (if (< (count r1) (count r2))
		   (conj (vec (cons (first pairs) (f (partition 2 (rest pairs))))) (last pairs))
		   (f (partition 2 1 r1))
		   ))))


(defn process
  [diamond]
  (if (< 1 (count diamond))
	(recur (cons (proc-row (take 2 diamond)) (drop 2 diamond)))
	(first (first diamond))))


(defn p81
  []
  (process (diamondify data)))


;(p81)

