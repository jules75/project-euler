(ns euler.p97)

(defn p97
    []
    (let [last10 #(mod % 10000000000)]
        (-> (iterate #(last10 (* 2 %)) 2)
            (nth (dec 7830457))
            (* 28433)
            inc
            last10
            )))
