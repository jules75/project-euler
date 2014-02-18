(ns euler.p85)

(defn nrects
  [w h]
  "Number of possible rectangles in w by h rect"
  (apply +
  (for [a (range w) b (range h)]
    (* (- w a) (- h b))
    ))
  )



(defn p85 []
  (let [rects (for [a (range 100) b (range 100) :when (< 1999000 (nrects a b) 2001000)]
             [a b (nrects a b)])
        [width height nrects] (first (sort-by #(Math/abs (- 2000000 (last %))) rects))
        ]
      (* width height)
    )
  )
