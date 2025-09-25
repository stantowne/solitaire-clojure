(ns solitaire-clojure.core
  (:gen-class)
  (:require [solitaire-clojure.print
             :refer [print-game-state]])
  (:require [solitaire-clojure.helper-functions
             :refer [force-card-face-up force-last-card-pile-face-up force-card-face-down]])
  (:require [solitaire-clojure.moves
             :refer [move-a-card-from-pile-to-foundation
                     move-a-card-from-waste-to-foundation
                     move-full-pile-to-different-pile
                     move-partial-pile-to-different-pile
                     move-a-card-from-waste-to-pile
                     flip]]))

;; Creates an unshuffled deck of cards
(def unshuffled-deck
  (vec (for [s (range 4) ; hearts 0, spades 1, diamonds 2, clubs 3
             v (range 1 14)]
         {:suit s :value v :face-up false})))

(defn shuffle-and-deal
  "shuffles the deck and then deals it out to initial condition"
  [deck]
  (let [sd (vec (shuffle deck))]
    {:field
      {
       :stock (subvec sd 0 24) ;; a single vector
       :waste [] ;; a single vector
       :tableau [[(force-card-face-up (sd 24))]
               [(sd 25) (force-card-face-up (sd 31))]
               [(sd 26) (sd 32) (force-card-face-up (sd 37))]
               [(sd 27) (sd 33) (sd 38) (force-card-face-up (sd 42))]
               [(sd 28) (sd 34) (sd 39) (sd 43) (force-card-face-up (sd 46))]
               [(sd 29) (sd 35) (sd 40) (sd 44) (sd 47) (force-card-face-up (sd 49))]
               [(sd 30) (sd 36) (sd 41) (sd 45) (sd 48) (sd 50) (force-card-face-up (sd 51))]] ;; a vector of 7 vectors
       :foundations [0, 0, 0, 0]} ;; a vector of 4 integers, representing the last card value in each foundation pile
     :moves-made 0 ;; an integer, representing the number of moves made so far
     :seen-fields #{} ;;a set of previous fields to detect loops
     }))
     ;; The result of this function is a map with three keys: :field, :moves-made, and :seen-fields
     ;; :field is itself a map with four keys: :stock, :waste, :tableau, and :foundations
       ;; :stock is a vector of cards (maps with keys :suit, :value, and :face-up)
       ;; :waste is a vector of cards (maps with keys :suit, :value, and :face-up)
       ;; :tableau is a vector of 7 vectors, each representing a pile of cards
       ;; :foundations is a vector of 4 integers, each representing the last card value in each foundation pile
     ;; :moves-made is an integer, representing the number of moves made so far
     ;; :seen-fields is a set of previous fields to detect loops



(defn play-game
  ([game-state]
  ((loop [game-state game-state]

     (cond

      (= (reduce + (:foundations (:field game-state)) 52)
      {:result :won}

      (or (= (:moves-made game-state) 200))
      {:result :lost-limit-reached}

      (contains? (:seen-states game-state) (:field game-state))
      {:result :lost-game-state-repeated}

      :else
         (let [new-seen-states (conj (:seen-states game-state) (:field game-state))]

           (let [result (move-a-card-from-pile-to-foundation game-state 2)] ;; aka move ace or deuce up
             (if (some? result)
               (recur result)
               ))

           (let [result (move-a-card-from-waste-to-foundation game-state 2)] ;; aka move ace or deuce over
             (if (some? result)
               (recur result)
             ))

           (let [result (move-full-pile-to-different-pile game-state)] ;; move full pile
             (if (some? result)
               (recur result)
             ))

           (let [result (move-partial-pile-to-different-pile game-state)] ;; move partial pile
             (if (some? result)
               (recur result)
             ))

           (let [result (move-a-card-from-pile-to-foundation game-state 13)] ;; move any card up
             (if (some? result)
               (recur result)
             ))

           (let [result (move-a-card-from-waste-to-pile game-state)]
             (if (some? result)
               (recur result)
             ))
           (let [result (move-a-card-from-waste-to-foundation game-state 13)] ;; move any card to foundation)))))






      ;;
      ;;
      ;;
      ;; flip
      ;; (recur (flip game-state) (inc moves-made) (conj seen-states game-state)))))
      :else
      {:result :not-implemented}))))))


(defn -main
  "Main entry point for the Solitaire game"
  []
  (let [game-state (shuffle-and-deal unshuffled-deck)]
    (print-game-state game-state)
    (print-game-state (flip game-state))
    (print-game-state (flip (flip game-state)))))