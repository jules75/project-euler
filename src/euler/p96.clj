(ns euler.p96
    [:require
      [euler.fns :as f]
     [clojure.set :refer [difference intersection]]
     [clojure.math.numeric-tower :refer [expt sqrt]]
     [clojure.string :refer [split-lines split]]]
 )


; note that this code can handle sudokus of different sizes, e.g. 4x4, 16x6
; seems slow and complex - perhaps a more elegant recursive solution?


(defn width
  [sudoku]
  "Return width of given sudoku (internal representation)"
  (-> sudoku count sqrt int))

(defn create
  [arr]
  "Import readable sudoku into internal representation"
  (map #(if (zero? %) (vec (range 1 (inc (count arr)))) [%]) (flatten arr)))

(defn export
  [sudoku]
  (partition (width sudoku) (map #(if (= 1 (count %)) (first %) 0) sudoku)))


(defn transform-cols
  [sudoku]
  "Make columns rows, call twice to get back to original rows"
  (let [n (sqrt (width sudoku))
		w (* n n)
		col-indices (mapcat #(take w (iterate (partial + w) %)) (range w))]
	(map #(nth sudoku %) col-indices)
    ))

(defn transform-boxes
  [sudoku]
  "Make boxes into rows, call twice to get back original rows"
  (let [n (sqrt (width sudoku))
		box-indices (for [a (range 0 (expt n 4) (expt n 3))
		  b (range a (+ a (expt n 2)) n)
		  c (range b (+ b (expt n 3)) (expt n 2))
		  d (range c (+ c n))]
		  d)]
	(map #(nth sudoku %) box-indices)
	))

(defn discard-singles
  [group]
  "Strategy: if square has single candidate, discard that candidate from other squares"
  (let [singles (->> group (filter #(= 1 (count %))) flatten)]
    (map #(if (< 1 (count %)) (vec (difference (set %) (set singles))) %) group)
    ))

(defn isolate-singles
  [group]
  "Strategy: if square has candidate no other square has, discard its other candidates"
  (let [uniques (->> group flatten frequencies (filter #(= 1 (val %))) (map first))]
    (for [g group]
      (let [i (intersection (set g) (set uniques))]
        (if (empty? i) g (vec i))))))

(defn discard-pairs
  [group]
  "Strategy: if two squares exists that contain only the same pair of candidates, discard
  those candidates from the other squares"
  (let [pairs (map key (filter #(and (= 2 (val %)) (= 2 (count (key %)))) (frequencies group)))]
    (for [square group]
      (if (contains? (set pairs) square)
        square
        (vec (difference (set square) (set (flatten pairs))))
        )
      )
    ))


(defn perform-strategy
	[strategy-fn sudoku]
	"Perform strategy-fn on every row, column and box to discard candidates
	Returns altered sudoku"
	(let [process #(mapcat strategy-fn (partition (width %) %))]
		(-> sudoku process
			transform-cols process transform-cols
			transform-boxes process transform-boxes)))


(defn solved?
  [sudoku]
  "True if sudoku is solved and legal"
  (->>
     (concat sudoku (transform-boxes sudoku) (transform-cols sudoku))
     flatten
   (partition (width sudoku))
   (map sort)
   (apply =)))

(def strategies
	(map #(partial perform-strategy %)
		[discard-singles isolate-singles discard-pairs]))

(defn stuck?
  [sudoku]
  "True if no progress to be made with any of the strategies"
  (= sudoku ((apply comp strategies) sudoku)))

(defn solve
  "Solve sudoku using single row/col/box strategies"
  ([sudoku fns]
   (if (or (solved? sudoku) (stuck? sudoku))
     sudoku
     (recur ((first fns) sudoku) (rest fns))))
  ([sudoku]
   (solve sudoku (flatten (repeat strategies)))))


(defn- fetch
  []
  "Read sudokus from project euler site"
  (->> (slurp "http://projecteuler.net/project/sudoku.txt")
       split-lines
       (remove #(re-find #"Grid" %))
       (map #(map (fn [c] (- (int c) 48)) %))
       (partition 9)
       )
  )

(defn isolate-candidates
  [sudoku]
  "Returns a lazy sequence of all possible variations on sudoku allowing
  each single candidate to exist in isolation. Useful when doing a brute force
  test for a solution. Recommended for mostly solved sudokus only."
  (let [indexed-cells (zipmap (range) sudoku)
        multi-cells (into {} (filter #(< 1 (count (val %))) indexed-cells))
        replacements (mapcat identity (for [c multi-cells] (map #(vector (key c) [%]) (val c))))]
        (if (pos? (count multi-cells))
          (map #(assoc-in (vec sudoku) (vector (first %)) (last %)) replacements)
          [sudoku]
          )
   ))

(defn p96
  []
  (apply +
  (for [s (fetch)]             ; for each sudoku
     (->> s create             ; import it
          solve                ; solve with traditional strategies
          isolate-candidates   ; brute force if not solved
          (map solve)
          (filter solved?)
          first
          (take 3) flatten     ; get first three digits
          f/undigits
          ))))



; TODO
; handle invalid sudokus
; clarify vocabulary (sudoku, arr, group, candidate, square)
