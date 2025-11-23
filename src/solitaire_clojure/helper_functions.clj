(ns solitaire-clojure.helper-functions
  (:require [solitaire-clojure.types :as t])
  (:import [solitaire_clojure.types Card]))


(defn force-card-face-up
  [^Card card]
  (assoc card :face-up true))

(defn force-last-card-pile-face-up
  "force the last card of a pile face up"
  [^clojure.lang.IPersistentVector pile]
  (if (empty? pile)
    pile
    (let [last-card (last pile)]
      (conj (vec (butlast pile)) (force-card-face-up last-card)))))

(defn force-card-face-down
  [^Card card]
  (assoc card :face-up false))

(defn color [^Card card]
  (if (odd? (.-suit card))
    :red
    :black))


(defn dif-color?
  "are the two cards of different colors?"
  [^Card card1 ^Card card2]
  (not= (color card1) (color card2)))

(comment
  (defn same-color?
    "are the two cards of the same color?"
    [^Card card1 ^Card card2]
    (= (color card1) (color card2)))
)

(defn sister-card?
  [^Card card1 ^Card card2]
  (and (= (.-value card1) (.-value card2))
       (= (color card1) (color card2))
       (not (= (.-suit card1) (.-suit card2)))))


(defn card-in-tableau-face-up?
  "accept a tableau (a vector of piles) and a card; return true if the card is face up in any pile"
  [^clojure.lang.IPersistentVector tableau ^Card card]
  (some (fn [pile]
          (some (fn [^Card x] (and (= x card) (.-face-up x))) pile)) ;; true if card is face up in this pile
        tableau)) ;; returns true if card is face up in any pile
        ;;
(defn index-of
  [^clojure.lang.IPersistentVector coll target]
  (first (keep-indexed (fn [idx item] (when (= item target) idx)) coll)))

(defn count-face-up-cards
  "counts the number of face-up cards in a pile"
  [^clojure.lang.IPersistentVector pile]
  (count (filter (fn [^Card c] (.-face-up c)) pile)))

(defn count-face-up-cards-piles
  "counts the number of face-up cards in tableau"
  [^clojure.lang.IPersistentVector piles]
  (reduce + (map count-face-up-cards piles)))
