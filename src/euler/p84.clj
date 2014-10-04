(ns euler.p84)

;
; WARNING!!!!
; This gives the right answer, even though the code is wrong!
; For example, the community-chest and chance fns return *random*
; cards, not cards in a shuffled sequence. Also, look at the (roll)
; call in those fns - they won't be random each time.
; Finally, the 'go to jail after three pairs' rule is ignored.
;

(defn roll
    "Random move from rolling two 4-sided dice"
    []
    (let [n (+ (inc (rand-int 4)) (inc (rand-int 4)))]
        {:type :rel :val n}
        ))

(def MV-GOTO-GO {:type :abs :val 0})
(def MV-GOTO-JAIL {:type :abs :val 10})
(def MV-GOTO-NEXT-RAIL {:type :next :val [5 15 25 35 45]})
(def MV-GOTO-NEXT-UTIL {:type :next :val [12 28 52]})

(defn community-chest
    "Returns RANDOM community chest card"
    []
    (let [cards [MV-GOTO-GO    
                 MV-GOTO-JAIL
                 ]]
        (rand-nth (concat cards (repeat 14 (roll))))
        ))

(defn chance 
    "Returns RANDOM chance card"
    []
    (let [cards [MV-GOTO-GO   
                 MV-GOTO-JAIL
                 {:type :abs :val 11 }     ; go to C1
                 {:type :abs :val 24 }     ; go to E3
                 {:type :abs :val 39 }     ; go to H2
                 {:type :abs :val 5 }      ; go to R1
                 MV-GOTO-NEXT-RAIL
                 MV-GOTO-NEXT-RAIL
                 MV-GOTO-NEXT-UTIL
                 {:type :rel :val -3}       ; go back 3
                 ]] 
        (rand-nth (concat cards (repeat 6 (roll))))
        ))

(defn goto-jail [] MV-GOTO-JAIL)

(def square-rules {0 roll   ; go
              1 roll 
              2 community-chest
              3 roll 
              4 roll 
              5 roll     ; rail
              6 roll 
              7 chance
              8 roll 
              9 roll
              10 roll    ; jail
              11 roll 
              12 roll    ; utility
              13 roll 
              14 roll 
              15 roll    ; rail
              16 roll 
              17 community-chest
              18 roll 
              19 roll
              20 roll 
              21 roll 
              22 chance
              23 roll 
              24 roll
              25 roll    ; rail
              26 roll 
              27 roll 
              28 roll    ; utility
              29 roll
              30 goto-jail
              31 roll 
              32 roll 
              33 community-chest
              34 roll
              35 roll    ; rail 
              36 chance
              37 roll 
              38 roll 
              39 roll
              })

(defn make-move
    "Adjust player position based on rule for their current position."
    [pos]
    (let [move ((get square-rules pos))
          mtype (:type move)
          value (:val move)
          result (case mtype
                    :abs    value
                    :rel    (+ pos value)
                    :next   (first (drop-while #(< % pos) value))
                     )]
            (mod result 40)
            ))

(defn p84
    []
    (->> 0
         (iterate make-move)
         (take 50000)
         frequencies
         (sort-by val)
         reverse
         (take 3)
         (map first)
         (apply str)
         Integer/parseInt
         ))
