(ns solitaire-clojure.print)

(comment
(defn suit-str
  "suit number as string in color"
  [suit]
  (if (odd? suit)
    (str "\u001b[31m-" (str suit) "\u001b[0m")  ;red -- hearts 3 and diamonds 1
    (str "\u001b[36m-" (str suit) "\u001b[0m"))) ;blue -- (for black) spades 2 and clubs 0
)

(declare print-tableau)

(defn suit-str
  "suit number as string in color"
  [suit]
  (case suit
    3 (str "\u001b[31mH\u001b[0m") ; hearts (red)
    1 (str "\u001b[31mD\u001b[0m") ; diamonds (red)
    2 (str "\u001b[36mS\u001b[0m") ; spades (blue)
    0 (str "\u001b[36mC\u001b[0m") ; clubs (blue)
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
  "4 character string representation of a card, including Up or Dn."
  [card]
  (str
    (value-str (:value card))
    (suit-str (:suit card))
    (if (:face-up card) "Up" "Dn")))

(defn print-game-state
  "Prints the current game state"
  [game-state]
  (let [{:keys [field moves-made game-number]} game-state
        stock (:stock field)
        waste (:waste field)
        tableau (:tableau field)
        foundations (:foundations field)]
  (println (str "Current Game State:  \nGame Number: " game-number))
  (println (str "Moves Made: " moves-made))
  (println "Stock:" (clojure.string/join " " (map card-str stock)))
  (println "Waste:" (clojure.string/join " " (map card-str waste)))
  (println (str "Foundations: "
                (nth foundations 0) " Clubs; "
                (nth foundations 1) " Diamonds; "
                (nth foundations 2) " Spades; "
                (nth foundations 3) " Hearts"))
  (println "Tableau:")
  (print-tableau tableau)
  (println)
  (println "---------------------"))
  ;; (read-line)
  )



(require '[clojure.string :as str])



(defn print-tableau
  "Prints a Solitaire tableau column-wise."
  [tableau]
  ;; 1. Find the height of the tallest column.
  (let [max-height (apply max (map count tableau))]
    ;; 2. Loop for each horizontal row to be printed.
    (println "            0     1     2     3     4     5     6")
    (doseq [row-index (range max-height)]
      ;; 3. For the current row, build a sequence of card strings.
      (let [row-cards (for [col-index (range 7)]
                        (let [;; Get the card at [column, row], or nil if it doesn't exist.
                              card (get-in tableau [col-index row-index])]
                          (if card
                            (card-str card)
                            "    "))) ; 4. Use a placeholder for empty spots.
            row-str (str/join "  " row-cards)]

        (println "          " row-str)))))
