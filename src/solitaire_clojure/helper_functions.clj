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

(defn color [card]
  (if (odd? (:suit card))
    :red
    :black))


(defn dif-color?
  "are the two cards of different colors?"
  [card1 card2]
  (not= (color card1) (color card2)))

(defn same-color?
  "are the two cards of the same color?"
  [card1 card2]
  (= (color card1) (color card2)))

(defn sister-card?
  [card1 card2]
  (and (= (:value card1) (:value card2))
       (= (color card1) (color card2))
       (not (= (:suit card1) (:suit card2)))))


(defn card-in-tableau-face-up?
  "accept a tableau (a vector of piles) and a card; return true if the card is face up in any pile"
  [tableau card]
  (some (fn [pile]
          (some (fn [x] (and (= x card) (:face-up x))) pile)) ;; true if card is face up in this pile
        tableau)) ;; returns true if card is face up in any pile
        ;;
(defn index-of
  [coll target]
  (first (keep-indexed (fn [idx item] (when (= item target) idx)) coll)))

(defn count-face-up-cards
  "counts the number of face-up cards in a pile"
  [pile]
  (count (filter :face-up pile)))

(defn count-face-up-cards-piles
  "counts the number of face-up cards in tableau"
  [piles]
  (reduce + (map count-face-up-cards piles)))
