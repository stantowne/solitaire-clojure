(ns solitaire-clojure.print
  (:require [clojure.string :as str]
            [solitaire-clojure.types :as t])
  (:import [solitaire_clojure.types GameState Card Field]))


(comment
(defn suit-str
  "suit number as string in color"
  [^long suit]
  (if (odd? suit)
    (str "\u001b[31m-" (str suit) "\u001b[0m")  ;red -- hearts 3 and diamonds 1
    (str "\u001b[36m-" (str suit) "\u001b[0m"))) ;blue -- (for black) spades 2 and clubs 0
)

(declare print-tableau)

(defn suit-str
  "suit number as string in color"
  [^long suit]
  (case suit
    3 (str "\u001b[31mH\u001b[0m") ; hearts (red)
    1 (str "\u001b[31mD\u001b[0m") ; diamonds (red)
    2 (str "\u001b[36mS\u001b[0m") ; spades (blue)
    0 (str "\u001b[36mC\u001b[0m") ; clubs (blue)
    (str suit)))


(defn value-str
  "1 character string representation of a card value.I"
  [^long v]
  (case v
    1 "A"
    10 "T"
    11 "J"
    12 "Q"
    13 "K"
    (str v)))

(defn card-str
  "4 character string representation of a card, including Up or Dn."
  [^Card card]
  (str
    (value-str (.-value card))
    (suit-str (.-suit card))
    (if (.-face-up card) "up" "dn")))

(defn print-game-state
  "Prints the current game state"
  [^GameState game-state]
  (let [deck-number (:deck-number game-state)
        moves-made (.-moves-made game-state)
        move-limit (.-move-limit game-state)
        game-result (.-game-result game-state)
        ^Field field (.-field game-state)
        stock (.-stock field)
        waste (.-waste field)
        tableau (.-tableau field)
        foundations (.-foundations field)]
  (println (str "Current Game State:  \nDeck Number: " deck-number))
  (println (str "Moves Made: " moves-made))
  (println "Stock:" (clojure.string/join " " (map card-str stock)))
  (println "Waste:" (clojure.string/join " " (map card-str waste)))
  (println (str "Foundations: "
                (:value (last (nth foundations 0))) " Clubs; "
                (:value (last (nth foundations 1))) " Diamonds; "
                (:value (last (nth foundations 2))) " Spades; "
                (:value (last (nth foundations 3))) " Hearts"))
  (println "Tableau:")
  (print-tableau tableau)
  (println)
  (println "---------------------"))
  ;; (read-line)
  )


(defn print-tableau
  "Prints a Solitaire tableau column-wise."
  [tableau]
  ;; 1. Find the height of the tallest column.
  (let [max-height (apply max 0 (map count tableau))]
    ;; 2. Loop for each horizontal row to be printed.
    (println "            0     1     2     3     4     5     6")
    (doseq [row-index (range max-height)]
      ;; 3. For the current row, build a sequence of card strinGameState.
      (let [row-cards (for [col-index (range 7)]
                        (let [;; Get the card at [column, row], or nil if it doesn't exist.
                              card (get-in tableau [col-index row-index])]
                          (if card
                            (card-str card)
                            "    "))) ; 4. Use a placeholder for empty spots.
            row-str (str/join "  " row-cards)]

        (println "          " row-str)))))
