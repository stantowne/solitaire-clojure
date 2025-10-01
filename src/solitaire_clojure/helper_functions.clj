(ns solitaire-clojure.helper-functions)


(defn force-card-face-up
  [card]
  (assoc card :face-up true))

(defn force-last-card-pile-face-up
  "force the last card of a pile face up"
  [pile]
  (if (empty? pile)
    pile
    (let [last-card (last pile)]
      (conj (vec (butlast pile)) (force-card-face-up last-card)))))

(defn force-card-face-down
  [card]
  (assoc card :face-up false))

(defn dif-color
  "are the two cards of different colors?"
  [card1 card2]
  (not= (even? (:suit card1)) (even? (:suit card2))))


(defn card-in-tableau-face-up
  "accept a tableau (a vector of piles) and a card; return true if the card is face up in any pile"
  [tableau card]
  (some (fn [pile]
          (some (fn [x] (and (= x card) (:face-up x))) pile)) ;; true if card is face up in this pile
        tableau)) ;; returns true if card is face up in any pile