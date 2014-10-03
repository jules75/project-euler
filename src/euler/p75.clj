(ns euler.p75
    (:require [euler.fns :as f]
              [euler.prime :as p])
    )

(defn multiples
    "Return multiples of n < limit"
    [limit n]
    (take-while #(< % limit) (iterate (partial + n) n)))

(defn gcd
    "Greatest common denominator, Euclid's method"
    [a b]
    (cond 
        (> a b)     (recur (- a b) b)
        (< a b)     (recur a (- b a))
        :else       a
        ))

(defn coprime?
    [a b]
    (= 1 (gcd a b)))

(defn pyth-primitive-triples
    "Use Euclid's method to generate all PRIMITVE pythagorean
    triples below limit"
    [limit]
    (for [n (range 1 (Math/sqrt limit))
          m (range (inc n) (Math/sqrt limit))      ; satisfies m>n
          :when (odd? (- m n))          
          :when (coprime? m n)
          :let [a (- (* m m) (* n n))
                b (* 2 m n)
                c (+ (* m m) (* n n))
                sum (+ a b c)]
          :when (< sum limit)
          ]
        sum
        ))
        
(defn p75 []
    (let [limit 1500000]
    (->> (pyth-primitive-triples limit)
         (mapcat (partial multiples limit))     ; non-primitive pythagorean perims
         frequencies                            ; count occurrences
         (filter #(= 1 (last %)))               ; only perims that occur once
         count
         )))
