(ns solitaire-clojure.logic
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [solitaire-clojure.config :refer [config]]
            [solitaire-clojure.flip :as flip]
            [solitaire-clojure.helper-functions :refer [count-face-up-cards-piles force-card-face-up]]
            [solitaire-clojure.move-entire-pile :as entire-pile]
            [solitaire-clojure.move-partial-pile :as partial-pile]
    ;; --- No more :refer ---
            [solitaire-clojure.moves :as moves]
            [solitaire-clojure.print :refer [print-game-state]]
            [solitaire-clojure.state :refer [game-state-atom]]
            [solitaire-clojure.types :as t])
  (:import [solitaire_clojure.types GameState Card Field]))

(defonce csv-reader-atom (atom nil))

;; drop instructs to drop the first N decks from the CSV file
(defn init-csv-reader [filepath first-deck-num]
  (reset! csv-reader-atom (drop first-deck-num (csv/read-csv (io/reader filepath)))))

(defn next-deck-from-csv []
  (when-let [lines @csv-reader-atom]
    (when (seq lines)
      (let [line (first lines)
            _ (swap! csv-reader-atom rest)
            deck (vec
                   (for [i (range 0 104 2)]
                     (let [value-int (Integer/parseInt (nth line i))
                           suit-int (Integer/parseInt (nth line (inc i)))]
                       (t/->Card value-int suit-int false)) ;; value suit face-up
                     ))]
        deck))))

(defn deal-next-deck [deck-num]
  ;; 'deck' is now a vector of Card RECORDS
  (let [deck (next-deck-from-csv)]
    (when deck
      (let [sd (vec (rseq deck)) ;; 'sd' is also a vector of Card RECORDS -- reversed to match the GO program
            field-record (t/->Field ;; Use the Field constructor
                           (subvec sd 0 24) ;; :stock
                           [] ;; :waste
                           [[(force-card-face-up (nth sd 51))] ;; :tableau -- matches the GO program
                            [(nth sd 50) (force-card-face-up (nth sd 44))]
                            [(nth sd 49) (nth sd 43) (force-card-face-up (nth sd 38))]
                            [(nth sd 48) (nth sd 42) (nth sd 37) (force-card-face-up (nth sd 33))]
                            [(nth sd 47) (nth sd 41) (nth sd 36) (nth sd 32) (force-card-face-up (nth sd 29))]
                            [(nth sd 46) (nth sd 40) (nth sd 35) (nth sd 31) (nth sd 28) (force-card-face-up (nth sd 26))]
                            [(nth sd 45) (nth sd 39) (nth sd 34) (nth sd 30) (nth sd 27) (nth sd 25) (force-card-face-up (nth sd 24))]]
                           [[] [] [] []]) ;; :foundations
            ]

        (t/->GameState field-record ;; :field
                       0 ;; :moves-made
                       [field-record] ;; :seen-fields
                       :in-progress ;; :game-result
                       (:move-limit config) ;; :move-limit
                       deck-num) ;; :deck-number
        ))))

(defn game-over? [^GameState current-state]
  (not= (.-game-result current-state) :in-progress))

(defn find-and-make-move
  ([^GameState current-state]
     (let [movers [#(moves/move-a-card-from-pile-to-foundations % 2 13)
                  #(moves/move-a-card-from-waste-to-foundations % 2)
                  entire-pile/move-entire-pile
                  moves/move-a-card-from-waste-to-pile
                  partial-pile/move-partial-pile
                  #(moves/move-a-card-from-pile-to-foundations % 13 7)
                  #(moves/move-a-card-from-waste-to-foundations % 13)
                  flip/flip]
         new-state (or (some #(% current-state) movers) current-state)]
       (if (not= new-state current-state) ;; check to see that a move was made
          (let [^GameState new-state new-state
                ^Field f (.-field new-state)
                seen-fields (.-seen-fields new-state)
                moves-made (:moves-made new-state)
                move-limit (:move-limit new-state)]
                (cond
                   (= (+ (count-face-up-cards-piles (.-foundations f)) (count-face-up-cards-piles (.-tableau f))) 52)
                   (assoc new-state :game-result :won)

                   (= moves-made move-limit)
                   (assoc new-state :game-result :lost-limit-reached)

                   (some #(= % f) (drop-last seen-fields))
                   (assoc new-state :game-result :lost-field-repeated)

                   :else
                   new-state))
         (assoc current-state :game-result :lost-no-moves-possible)))))


(defn calculate-next-state [^GameState current-state]
  (if (game-over? current-state)
    current-state
    (find-and-make-move current-state)))

(defn back-one-move [^GameState current-state]
  (let [moves-made (:moves-made current-state)
        seen-fields (.-seen-fields current-state)]
    ;; 1. Your check was correct. We need at least 2 items:
    ;;    the current state and the one to go back to.
    (if (< (count seen-fields) 2)
      current-state ;; Can't go back

      ;; 2. If we can go back:
      (let [previous-seen-fields (vec (butlast seen-fields))
            previous-field (last previous-seen-fields)
            previous-moves-made (dec moves-made)
            previous-state (assoc current-state
                             :field previous-field
                             :seen-fields previous-seen-fields
                             :moves-made previous-moves-made
                             :game-result :in-progress)]
        previous-state))))


;; This is the fast, non-interactive "bot" player
(defn play-game []
  (loop []
    (let [^GameState state @game-state-atom]
    (when (:print-each-move? config)
      (print-game-state state))
    (if (game-over? state)
      {:result (.-game-result state)}
      (do
        (swap! game-state-atom calculate-next-state)
        (recur))))))