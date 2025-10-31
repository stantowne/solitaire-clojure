(ns solitaire-clojure.logic
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [solitaire-clojure.print :refer [print-game-state]]
            [solitaire-clojure.helper-functions :refer [force-card-face-up count-face-up-cards-piles]]
            [solitaire-clojure.config :refer [config]]
            [solitaire-clojure.state :refer [game-state-atom]]
    ;; --- No more :refer ---
            [solitaire-clojure.moves :as moves]
            [solitaire-clojure.flip :as flip]
            [solitaire-clojure.move-partial-pile :as partial-pile]
            [solitaire-clojure.move-entire-pile :as entire-pile]))

(defonce csv-reader-atom (atom nil))

(defn init-csv-reader [filepath first-deck-num]
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
      (let [sd (vec (rseq deck)) ;; sd is the shuffled deck reversed (to match the GO program)
            field-map {:stock       (subvec sd 0 24)
                       :waste       []
                       :tableau     [[(force-card-face-up (nth sd 51))]
                                     [(nth sd 50) (force-card-face-up (nth sd 44))]
                                     [(nth sd 49) (nth sd 43) (force-card-face-up (nth sd 38))]
                                     [(nth sd 48) (nth sd 42) (nth sd 37) (force-card-face-up (nth sd 33))]
                                     [(nth sd 47) (nth sd 41) (nth sd 36) (nth sd 32) (force-card-face-up (nth sd 29))]
                                     [(nth sd 46) (nth sd 40) (nth sd 35) (nth sd 31) (nth sd 28) (force-card-face-up (nth sd 26))]
                                     [(nth sd 45) (nth sd 39) (nth sd 34) (nth sd 30) (nth sd 27) (nth sd 25) (force-card-face-up (nth sd 24))]]
                       :foundations [[] [] [] []]}]
        {:field        field-map
         :moves-made   0
         :seen-fields  [field-map]
         :game-result  :in-progress
         :move-limit   (:move-limit config)}))))

(defn game-over? [current-state]
  (not= (:game-result current-state) :in-progress))

(defn find-and-make-move
  ([current-state]
     (let [movers [#(moves/move-a-card-from-pile-to-foundations % 2 13)
                  #(moves/move-a-card-from-waste-to-foundations % 2)
                  moves/move-a-card-from-waste-to-pile
                  entire-pile/move-entire-pile
                  partial-pile/move-partial-pile
                  #(moves/move-a-card-from-pile-to-foundations % 13 9)
                  #(moves/move-a-card-from-waste-to-foundations % 13)
                  flip/flip]
         new-state (or (some #(% current-state) movers) current-state)]
       (if (not= new-state current-state) ;; check to see that a move was made
          (let [{:keys [field seen-fields moves-made move-limit]} new-state]
                (cond
                   (= (+ (count-face-up-cards-piles (:foundations field)) (count-face-up-cards-piles (:tableau field))) 52)
                   (assoc new-state :game-result :won)

                   (= moves-made move-limit)
                   (assoc new-state :game-result :lost-limit-reached)

                   (some #(= % field) (drop-last seen-fields))
                   (assoc new-state :game-result :lost-field-repeated)

                   :else
                   new-state))
         (assoc current-state :game-result :lost-no-moves-possible)))))


(defn calculate-next-state [current-state]
  (if (game-over? current-state)
    current-state
    (find-and-make-move current-state)))

(defn back-one-move [current-state]
  (let [{:keys [moves-made seen-fields]} current-state]
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
    (when (:print-each-move? config)
      (print-game-state @game-state-atom))
    (if (game-over? @game-state-atom)
      {:result (:game-result @game-state-atom)}
      (do
        (swap! game-state-atom calculate-next-state)
        (recur)))))