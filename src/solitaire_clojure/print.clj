(ns solitaire-clojure.print)

(defn suit-str
  "suit number as string in color"
  [suit]
  (if (even? suit))
    (str "\u001b[31m-" str(suit) "\u001b[0m")  ;red -- hearts 0 and diamonds 2
    (str "\u001b[36m-" str(suit) "\u001b[0m")) ;blue -- (for black) spades 1 and clubs 3

(defn value-str
  "1 character string representation of a card value."
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
  (println "Current Game State:")
  (println "Stock:" (clojure.string/join " " (map card-str(:stock game-state))))
  (println "Waste:" (clojure.string/join " " (map card-str (:waste game-state))))
  (println "Tableau:")
  (doseq [pile (:tableau game-state)]
    (println (clojure.string/join " " (map card-str pile))))
  (println "Foundations:")
  (doseq [pile (:foundations game-state)]
    (println (clojure.string/join " " (map card-str pile))))
  (println "---------------------"))