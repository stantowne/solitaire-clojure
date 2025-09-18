(ns solitaire-clojure.core
  (:gen-class)
  (:require [solitaire-clojure.print :refer [print-game-state]]))

;; Creates an unshuffled deck of cards
(def unshuffled-deck
  (vec (for [s (range 4) ; hearts 0, spades 1, diamonds 2, clubs 3
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
     :foundations [0, 0, 0, 0]})) ;; a vector of 4 integers, representing the last card value in each foundation pile

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

(defn play-game
  ([game-state] (play-game game-state 0 #{})) ; single arity version (start of game) calls 3 arity version to start
  ([game-state moves-made seen-states] ; three arity version
   (loop [game-state game-state
          moves-made moves-made
          seen-states seen-states]

     (cond

      (= (reduce + :foundations game-state) 52)
      {:result :won}

      (or (= moves-made 200))
      {:result :lost-limit-reached}

      (contains? seen-states game-state)
      {:result :lost-game-state-repeated}

      ;; ace-up
      ;; recur
      ;;
      ;; deuce-up
      ;; recur
      ;;
      ;; ace-across
      ;; if the last card in the waste is an ace
      (if (= (:value (last (:waste game-state))) 1))
      (let [suit-number (:suit (last (:waste game-state)))
            new-foundation (update (:foundations game-state) suit-number inc)
            new-waste (vec (butlast (:waste game-state)))
            new-game-state (assoc game-state :foundations new-foundation :waste new-waste)]
          (recur new-game-state (inc moves-made) (conj seen-states new-game-state))))

      ;; deuce-across
      ;; if the last card in the waste is a deuce and the foundation for that suit is at 1
      (if (and (= (:value (last (:waste game-state))) 2) (= (:suit (last (:waste game-state))) 1))
      (let [suit-number (:suit (last (:waste game-state)))
            new-foundation (update (:foundations game-state) suit-number inc)
            new-waste (vec (butlast (:waste game-state)))
            new-game-state (assoc game-state :foundations new-foundation :waste new-waste)]
       (recur new-game-state (inc moves-made) (conj seen-states new-game-state))))
      ;;
      ;; full-tableau-to-tableau
      ;; recur
      ;;
      ;; partial-tableau-to-tableau
      ;; recur
      ;;
      ;; waste-down
      ;; recur
      ;;
      ;; waste-across
      ;;
      ;; flip
      ;; (recur (flip game-state) (inc moves-made) (conj seen-states game-state)))))
      :else
      {:result :not-implemented})))


(defn -main
  "Main entry point for the Solitaire game"
  []
  (let [game-state (shuffle-and-deal unshuffled-deck)]
    (print-game-state game-state)
    (print-game-state (flip game-state))
    (print-game-state (flip (flip game-state)))))