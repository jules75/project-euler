(ns euler.p50
	(:require [euler.fns :as f]
		[euler.prime :as p]
		[clojure.pprint :as pp]))
		
(def primes-to-1k [
	2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 
	83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 
	173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 
	263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 
	359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 
	457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 
	569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 
	659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 
	769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 
	881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 
	997])
 
(def primes-to-1m (p/sieve primes-to-1k (cons 2 (range 3 1000000 2))))

(defn prime-sums
	[start max]
	"Returns sums of all sequences of consecutive primes
	2 100 => 2 5 10 17 28 41 58 77
	19 100 => 19 42 71 102 139 180"
	(->> primes-to-1m
		(drop-while #(< % start))
		(reductions +)
		(take-while #(< % max))))

(defn prime-sums-max
	[start max]
	"Calculate longest sequence of consecutive primes not exceeding max
	whose sum is prime. Returns [length sum], [0 0] if none found."
	(let [s (prime-sums start max)]
		(if (seq s)
			(->>
				(zipmap (-> s count inc range rest) s)		; attach counts to sums
				(filter #(p/prime? (last %)))				; keep only primes
				(reduce #(if (> (key %1) (key %2)) %1 %2))	; find longest series
				) [0 0])))

(defn p50 []
	(let [limit 1000000]
		(->> (range (Math/sqrt (/ limit 2)))				; analysis shows len<sqrt(lim/2)
			(map #(prime-sums-max % limit))
			(reduce #(if (> (first %) (first %2)) % %2))	; find longest series
			last)))
