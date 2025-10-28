(ns solitaire-clojure.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [solitaire-clojure.print :refer [print-game-state]]
            [solitaire-clojure.helper-functions :refer [force-card-face-up count-face-up-cards-piles]]
            [solitaire-clojure.moves :refer [move-a-card-from-pile-to-foundations
                                             move-a-card-from-waste-to-foundations
                                             move-a-card-from-waste-to-pile]]
            [solitaire-clojure.flip :refer [flip]]
            [solitaire-clojure.config :refer [config]]
            [solitaire-clojure.move-partial-pile :refer [move-partial-pile]]
            [solitaire-clojure.move-entire-pile :refer [move-entire-pile]]
            [solitaire-clojure.state :refer [game-state-atom]]
            [solitaire-clojure.ui :as ui]))


(defonce csv-reader-atom (atom nil))

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
                        {:stock       (subvec sd 0 24)
                         :waste       []
                         :tableau     [[(force-card-face-up (nth sd 51))]
                                       [(nth sd 50) (force-card-face-up (nth sd 44))]
                                       [(nth sd 49) (nth sd 43) (force-card-face-up (nth sd 38))]
                                       [(nth sd 48) (nth sd 42) (nth sd 37) (force-card-face-up (nth sd 33))]
                                       [(nth sd 47) (nth sd 41) (nth sd 36) (nth sd 32) (force-card-face-up (nth sd 29))]
                                       [(nth sd 46) (nth sd 40) (nth sd 35) (nth sd 31) (nth sd 28) (force-card-face-up (nth sd 26))]
                                       [(nth sd 45) (nth sd 39) (nth sd 34) (nth sd 30) (nth sd 27) (nth sd 25) (force-card-face-up (nth sd 24))]]
                         :foundations [[] [] [] []]} ;; Clubs, Diamonds, Spades, Hearts
                        :moves-made 0
                        :seen-fields [] ;; all prior fields. used to detect loops.
                        :game-result :in-progress
                        :move-limit (:move-limit config)}] ;; added so that find-and-make-move is pure
        game-state))))


(defn game-over? [current-state]
  (not= (:game-result current-state) :in-progress))

(defn find-and-make-move
  ([current-state]
   (let [{:keys [field moves-made seen-fields move-limit]} current-state]
     (cond
       (= (+ (count-face-up-cards-piles (:foundations field)) (count-face-up-cards-piles (:tableau field))) 52)
       (assoc current-state :game-result :won)

       (= moves-made move-limit)
       (assoc current-state :game-result :lost-limit-reached)

       (some #(= % field) seen-fields)
       (assoc current-state :game-result :lost-field-repeated)

       :else
       (let [new-seen-fields (conj seen-fields field)
             current-state (assoc current-state :seen-fields new-seen-fields)
             movers [#(move-a-card-from-pile-to-foundations % 2 13)
                     #(move-a-card-from-waste-to-foundations % 2)
                     move-a-card-from-waste-to-pile
                     move-entire-pile
                     move-partial-pile
                     #(move-a-card-from-pile-to-foundations % 13 9)
                     #(move-a-card-from-waste-to-foundations % 13)
                     flip]]
         (or (some #(% current-state) movers)
             current-state))))))

(defn calculate-next-state [current-state]
  (if (game-over? current-state)
    current-state
    (find-and-make-move current-state)))

(defn play-game []
  (loop []
    (when (:print-each-move? config)
      (print-game-state @game-state-atom))
    (if (game-over? @game-state-atom)
      {:result (:game-result @game-state-atom)}
      (do
        (swap! game-state-atom calculate-next-state)
        (recur)))))

(defn play-game-interactive []
  (println "--- New Game (Interactive) ---")
  (loop [] ;; This is the main "per-move" loop

    ;; 1. Print the current state *once*
    (print-game-state @game-state-atom)

    ;; 2. Check if the game is over
    (if (game-over? @game-state-atom)
      (do
        (println "--- Game Over ---")
        {:result (:game-result @game-state-atom)})

      ;; 3. Game is not over, start the "get-input" loop
      (let [input (loop [] ;; This is the inner "input-validation" loop
                    (println "\n<Enter> to make next move, (g)ive up on this game, e(x)it program:")
                    (let [in (read-line)]
                      (if (or (= in "") (= in "g") (= in "x"))
                        in ;; Valid input, return it from the inner loop
                        (do
                          (println "Invalid input, please try again.")
                          (recur)))))] ;; This recur *only* repeats the input prompt

        ;; 4. We now have valid input ("m" or "g"), so we act
        (cond
          (= input "")
          (do
            (swap! game-state-atom calculate-next-state)
            (recur)) ;; This recur goes back to the main "per-move" loop

          (= input "g")
          {:result :quit-by-user} ;; Quit the game

          (= input "x")
          {:result :exit-program}
          )))))

(defn -main
  "Main entry point for the Solitaire game"
  []
  (init-csv-reader (:decks-filepath config) (:first-deck-num config))
  (when (:interactive-mode? config)
    (ui/launch-ui))
  (let [[_ final-results]
         (loop [deck-number (:first-deck-num config)
                record-of-results {:lost-limit-reached 0 :lost-field-repeated 0 :won 0}] ;; quite-by-user not initialized
          (if (< deck-number (+ (:first-deck-num config) (:num-of-decks config)))
            (let [initial-game-map (deal-next-deck)
                  initial-game-state (assoc initial-game-map :deck-number deck-number)
                  _ (reset! game-state-atom initial-game-state)
                  result (if (:interactive-mode? config)
                           (play-game-interactive)
                           (play-game))
                  updated-results
                    (cond
                      (= (:result result) :won)
                      (do
                        (when (:interactive-mode? config)
                          (println (str "--- Deck " deck-number " Won. ---")))
                        (update record-of-results :won inc))

                      (= (:result result) :lost-limit-reached)
                      (do
                        (when (:interactive-mode? config)
                          (println (str "--- Deck " deck-number " Lost (move limit). ---")))
                        (update record-of-results :lost-limit-reached inc))

                      (= (:result result) :lost-field-repeated)
                      (do
                        (when (:interactive-mode? config)
                          (println (str "--- Deck " deck-number " Lost (loop detected). ---")))
                        (update record-of-results :lost-field-repeated inc))

                      (= (:result result) :quit-by-user)
                      (do
                        (println (str "--- Deck " deck-number " skipped by user. ---"))
                        (update record-of-results :quit-by-user (fnil inc 0)))

                      (= (:result result) :exit-program)
                      (do
                        (println (str "--- Exiting program at user request. Final results below. ---"))
                        record-of-results)

                      :else
                        (do
                        (println "Unexpected result:" result)
                        record-of-results))]

            (when (= (:result result) :won)
              (spit "decks-won-clojure.txt" (str "\nDeck number " deck-number " won.") :append true))

            (if (= (:result result) :exit-program)
              [deck-number updated-results]
              (recur (inc deck-number) updated-results)))

            [deck-number record-of-results]))]
  (println "Record of Results:" final-results)
  (System/exit 0)))
