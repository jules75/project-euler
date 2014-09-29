(ns euler.p59
	(:require [clojure.string :as s]))

(def cipher-nums
  (let [raw (slurp "https://projecteuler.net/project/resources/p059_cipher.txt")
   tokens (s/split (s/trim raw) #",")]
  (map #(Integer/parseInt %) tokens)
  ))

(def passwords
  "Lazy sequence of all possible passwords (as numbers)"
  (let [nums (range 97 123)]
    (for [a nums b nums c nums]
      (vector a b c)
      )))

(defn xor
  [cipher password]
  "Cipher and password are integers, password will be repeated
  Returns integers"
  (map bit-xor cipher (cycle password))
  )

(defn looks-english?
  [string]
  (boolean (re-find #" the " string))
  )

(defn p59 []
  (->> passwords
   (map #(xor cipher-nums %))
   (map #(map char %))
   (map #(apply str %))
   (filter looks-english?)
   first
   (map int)
   (apply +)
   ))
