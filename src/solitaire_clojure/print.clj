(ns solitaire-clojure.print)

(comment
(defn suit-str
  "suit number as string in color"
  [suit]
  (if (even? suit)
    (str "\u001b[31m-" (str suit) "\u001b[0m")  ;red -- hearts 0 and diamonds 2
    (str "\u001b[36m-" (str suit) "\u001b[0m"))) ;blue -- (for black) spades 1 and clubs 3
)

(defn suit-str
  "suit number as string in color"
  [suit]
  (case suit
    0 (str "\u001b[31mH\u001b[0m") ; hearts (red)
    2 (str "\u001b[31mD\u001b[0m") ; diamonds (red)
    1 (str "\u001b[36mS\u001b[0m") ; spades (blue)
    3 (str "\u001b[36mC\u001b[0m") ; clubs (blue)
    (str suit)))


(defn value-str
  "1 character string representation of a card value.I"
  [v]
  (case v
    1 "A"
    10 "T"
    11 "J"
    12 "Q"
    13 "K"
    (str v)))

(defn card-str
  "3 character string representation of a card, including face up or down."
  [card]
  (str
    (value-str (:value card))
    (suit-str (:suit card))
    (if (:face-up card) "Up" "Dn")))

(defn print-game-state
  "Prints the current game state"
  [game-state]
  (let [{:keys [field moves-made]} game-state
        stock (:stock field)
        waste (:waste field)
        tableau (:tableau field)
        foundations (:foundations field)]
  (println "Current Game State:")
  (println "Stock:" (clojure.string/join " " (map card-str stock)))
  (println "Waste:" (clojure.string/join " " (map card-str waste)))
  (println "Tableau:")
  (doseq [pile tableau]
    (println (str "  " (clojure.string/join " " (map card-str pile)))))
  (println (str "Foundations: "
            (nth foundations 0) " Hearts; "
            (nth foundations 1) " Spades; "
            (nth foundations 2) " Diamonds; "
            (nth foundations 3) " Clubs"))
  (println "Moves made:" moves-made)
  (println "---------------------"))
  ;; (read-line)
  )