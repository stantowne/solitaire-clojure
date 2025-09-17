(ns solitaire-clojure.print)

(def suit-symbol
  {:hearts "Hrt"
   :diamonds "Dia"
   :clubs "Clb"
   :spades "Spd"})

(defn suit-str
  "1-character string representation of a card suit, color hearts and diamonds red."
  [suit]
  (let [symbol (get suit-symbol suit "?")]
    (if (#{:hearts :diamonds} suit)
      (str "\u001b[31m" symbol "\u001b[0m")
      (str "\u001b[36m" symbol "\u001b[0m"))))

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