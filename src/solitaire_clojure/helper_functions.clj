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