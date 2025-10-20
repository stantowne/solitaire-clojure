(ns solitaire-clojure.core
  (:gen-class)
  (:require
    [solitaire-clojure.print :refer [print-game-state]]
    [solitaire-clojure.helper-functions :refer [force-card-face-up]]
    [solitaire-clojure.moves :refer [move-a-card-from-pile-to-foundations
                                     move-a-card-from-waste-to-foundations
                                     move-a-card-from-waste-to-pile]]
    [solitaire-clojure.move-partial-pile :refer [move-partial-pile]]
    [solitaire-clojure.move-entire-pile :refer [move-entire-pile]]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [solitaire-clojure.flip :refer [flip]]))

(defonce csv-reader-atom (atom nil))

(def config
  {:decks-filepath "test/resources/decks-made-2022-01-15-count-10000-dict.csv"
   :first-deck-num 1 ; files are treated as 0 base
   :num-of-decks 1 ;; do not exceed number of decks minus first-deck-num
   :move-limit 200
   :print-if-move-count-exceeds -1
   :print-func-failure false})



(defn init-csv-reader [filepath first-deck-num] ;; "resources/decks-made-2022-01=15-count-10000-dict.csv"
  (reset! csv-reader-atom (drop first-deck-num (csv/read-csv (io/reader filepath)))))

(defn next-deck-from-csv []
  (when-let [lines @csv-reader-atom]
    (when (seq lines)
      (let [line (first lines)
            _ (swap! csv-reader-atom rest)
            deck (vec
                   (for [i (range 0 104 2)]
                     {:value (Integer/parseInt (nth line i))
                      :suit (Integer/parseInt (nth line (inc i)))
                      :face-up false}))]
        deck))))

(defn deal-next-deck []
  (let [deck (next-deck-from-csv)]
    (when deck
      (let [sd (vec (rseq deck)) ;; reverse the deck to deal from the end and to conform to GO program
            game-state {:field
                        {:stock       []
                         :waste       (vec (map force-card-face-up (reverse (subvec sd 0 24))))
                         :tableau     [[(force-card-face-up (nth sd 51))]
                                       [(nth sd 50) (force-card-face-up (nth sd 44))]
                                       [(nth sd 49) (nth sd 43) (force-card-face-up (nth sd 38))]
                                       [(nth sd 48) (nth sd 42) (nth sd 37) (force-card-face-up (nth sd 33))]
                                       [(nth sd 47) (nth sd 41) (nth sd 36) (nth sd 32) (force-card-face-up (nth sd 29))]
                                       [(nth sd 46) (nth sd 40) (nth sd 35) (nth sd 31) (nth sd 28) (force-card-face-up (nth sd 26))]
                                       [(nth sd 45) (nth sd 39) (nth sd 34) (nth sd 30) (nth sd 27) (nth sd 25) (force-card-face-up (nth sd 24))]]
                         :foundations [0 0 0 0]}
                        :moves-made 0
                        :seen-fields []
                        :initial-flip-count-at-start-of-round 8}]
        game-state))))

(comment
  ;; Creates an unshuffled deck of cards
  (def unshuffled-deck
    (vec (for [s (range 4) ; hearts 0, spades 1, diamonds 2, clubs 3
               v (range 1 14)]
           {:suit s :value v :face-up false})))

  (defn shuffle-and-deal
    "Returns a map with three first level keys -- :field, :moves-made, and :seen-fields. Shuffles the deck and then deals it out to initial condition"
    [deck]
    (let [sd (vec (shuffle deck))
          game-state {:field
                      {:stock       []
                       :waste       (subvec sd 0 24) ; as if 8 initial flips had been made
                       :tableau     [[(force-card-face-up (nth sd 51))]
                                     [(nth sd 50) (force-card-face-up (nth sd 44))]
                                     [(nth sd 49) (nth sd 43) (force-card-face-up (nth sd 38))]
                                     [(nth sd 48) (nth sd 42) (nth sd 37) (force-card-face-up (nth sd 33))]
                                     [(nth sd 47) (nth sd 41) (nth sd 36) (nth sd 32) (force-card-face-up (nth sd 29))]
                                     [(nth sd 46) (nth sd 40) (nth sd 35) (nth sd 31) (nth sd 28) (force-card-face-up (nth sd 26))]
                                     [(nth sd 45) (nth sd 39) (nth sd 34) (nth sd 30) (nth sd 27) (nth sd 25) (force-card-face-up (nth sd 24))]]
                       :foundations [0 0 0 0]}
                      :moves-made  0
                      :seen-fields []
                      :initial-flip-count-at-start-of-round 8}]
      game-state))
  )
;; The result of this function is a map with three keys: :field, :moves-made, and :seen-fields
;; :field is itself a map with four keys: :stock, :waste, :tableau, and :foundations
 ;; :stock is a vector of cards (maps with keys :suit, :value, and :face-up)
 ;; :waste is a vector of cards (maps with keys :suit, :value, and :face-up)
 ;; :tableau is a vector of 7 vectors, each representing a pile of cards
 ;; :foundations is a vector of 4 integers, each representing the last card value in each foundation pile
;; :moves-made is an integer, representing the number of moves made so far
;; :seen-fields is a set of previous fields to detect loops
;;




(defn play-game
  ([game-state]
   (loop [game-state game-state]
     (when (:print-game-state-each-move config) (print-game-state game-state))
     (cond
      (= (reduce + (:foundations (:field game-state))) 52)
        {:result :won}
      (= (:moves-made game-state) (:move-limit config))
        {:result :lost-limit-reached}
      (some #(= % (:field game-state)) (:seen-fields game-state))
        {:result :lost-field-repeated}
      :else
         (let [new-seen-fields (conj (:seen-fields game-state) (:field game-state))
               game-state (assoc game-state :seen-fields new-seen-fields)]
            (if-let [result (move-a-card-from-pile-to-foundations game-state 2 13)]
              (recur result)
              (do
                (when (:print-func-failure config) (println "move-a-card-from-pile-to-foundations (2) failed"))
                (if-let [result (move-a-card-from-waste-to-foundations game-state 2)]
                  (recur result)
                  (do
                    (when (:print-func-failure config) (println "move-a-card-from-waste-to-foundations (2) failed"))
                    (if-let [result (move-a-card-from-waste-to-pile game-state)]
                      (recur result)
                      (do
                        (when (:print-func-failure config) (println "move-a-card-from-waste-to-pile failed"))
                        (if-let [result (move-entire-pile game-state)]
                          (recur result)
                          (do
                            (when (:print-func-failure config) (println "move-entire-pile failed"))
                            (if-let [result (move-partial-pile game-state)]
                              (recur result)
                              (do
                                (when (:print-func-failure config) (println "move-partial-pile failed"))
                                (if-let [result (move-a-card-from-pile-to-foundations game-state 13 9)]
                                  (recur result)
                                  (do
                                    (when (:print-func-failure config) (println "move-a-card-from-pile-to-foundations (13) failed"))
                                    (if-let [result (move-a-card-from-waste-to-foundations game-state 13)]
                                      (recur result)
                                      (do
                                        (when (:print-func-failure config) (println "move-a-card-from-waste-to-foundations (13) failed"))
                                        (if-let [result (flip game-state)]
                                          (recur result)
                                          (do
                                            (when (:print-func-failure config) (println "flip failed"))
                                            game-state)))))))))))))))))))))

(defn -main
  "Main entry point for the Solitaire game"
  []
  (init-csv-reader (:decks-filepath config) (:first-deck-num config))
  (let [[_ final-results]
         (loop [deck-number (:first-deck-num config)
                record-of-results {:lost-limit-reached 0 :lost-field-repeated 0 :won 0}]
           (when (:print-each-deck-num-as-it-starts config)(println "Starting deck number:" deck-number))
           (if (< deck-number (+ (:first-deck-num config) (:num-of-decks config))) ;; change to 10000 for full run
             (let [game-state (assoc (deal-next-deck) :deck-number deck-number)
                   result (play-game game-state)
                   updated-results
                   (cond
                     (= (:result result) :won) (update record-of-results :won inc)
                     (= (:result result) :lost-limit-reached) (update record-of-results :lost-limit-reached inc)
                     (= (:result result) :lost-field-repeated) (update record-of-results :lost-field-repeated inc)
                     :else
                     (do
                       (println "Unexpected result:" result)
                       record-of-results))]
               (when (= (:result result) :won)
                 (spit "decks-won-clojure.txt" (str "\nDeck number " deck-number " won.") :append true))
               (recur (inc deck-number) updated-results))
             [deck-number record-of-results]))]
    (println "Record of Results:" final-results)))