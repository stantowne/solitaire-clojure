(ns solitaire-clojure.core
  (:gen-class)
  (:require [solitaire-clojure.print :refer [print-game-state]]))

(def suits [:hearts :diamonds :clubs :spades])

;; Creates an unshuffled deck of cards
(def unshuffled-deck
  (vec (for [s suits
             v (range 1 14)]
         {:suit s :value v :face-up false})))

(defn force-card-face-up
  [card]
  (assoc card :face-up true))

(defn force-card-face-down
  [card]
  (assoc card :face-up false))

(defn shuffle-and-deal
  "shuffles the deck and then deals it out to initial condition"
  [deck]
  (let [sd (vec (shuffle deck))]
    {:stock (subvec sd 0 24) ;; a single vector
     :waste [] ;; a single vector
     :tableau [[(force-card-face-up (sd 24))]
               [(sd 25) (force-card-face-up (sd 31))]
               [(sd 26) (sd 32) (force-card-face-up (sd 37))]
               [(sd 27) (sd 33) (sd 38) (force-card-face-up (sd 42))]
               [(sd 28) (sd 34) (sd 39) (sd 43) (force-card-face-up (sd 46))]
               [(sd 29) (sd 35) (sd 40) (sd 44) (sd 47) (force-card-face-up (sd 49))]
               [(sd 30) (sd 36) (sd 41) (sd 45) (sd 48) (sd 50) (force-card-face-up (sd 51))]] ;; a vector of 7 vectors
     :foundations [[], [], [], []]})) ;; a vector of 4 vectors

(defn flip ;; the first semi-complicated function written by LST with almost no help from AI!
  "'Flips' the stock and waste piles according to Klondike rules"
  [game-state]
  (let [stock (:stock game-state)
        waste (:waste game-state)
        stock-count (count stock)
        waste-count (count waste)]

    (cond
      (and (= stock-count 0) (= waste-count 0))
        game-state ;; no change

      (= stock-count 0)
        (let [new-stock (mapv force-card-face-down (vec (reverse waste)))
             new-waste []]
        (assoc game-state :stock new-stock :waste new-waste))

      (> stock-count 2)
        (let [new-stock (subvec stock 0 (- stock-count 3))
             new-waste (vec (concat waste (map force-card-face-up (reverse (subvec stock (- stock-count 3) stock-count)))))]
        (assoc game-state :stock new-stock :waste new-waste))

      :else ;; 1 or 2 cards left in stock
        (let [new-stock []
              new-waste (vec (concat waste (map force-card-face-up (reverse (stock)))))]
           (assoc game-state :stock new-stock :waste new-waste)))))

(defn -main
  "Main entry point for the Solitaire game"
  []
  (let [game-state (shuffle-and-deal unshuffled-deck)]
    (print-game-state game-state)
    (print-game-state (flip game-state))
    (print-game-state (flip (flip game-state)))))