(ns solitaire-clojure.core
  (:gen-class)
  (:require [solitaire-clojure.config :refer [config]]
            [solitaire-clojure.state :refer [game-state-atom]]
            [solitaire-clojure.logic :as game]
            [solitaire-clojure.ui :as ui]))

(defn -main
  "Main entry point for the Solitaire game"
  []
  ;; 1. Initialize the CSV reader once for both modes
  (game/init-csv-reader (:decks-filepath config) (:first-deck-num config))

  (if (:interactive-mode? config)

    ;; --- PATH 1: INTERACTIVE UI MODE ---
    (do
      (println "Starting in interactive (UI) mode...")
      ;; Launch the UI window
      (ui/launch-ui) ; [cite: 69]
      ;; Deal the first deck and load it into the atom
      (let [initial-game-map (game/deal-next-deck)

            ;; --- ADDED NIL CHECK FOR INTERACTIVE ---
            _ (when (nil? initial-game-map)
                (throw (Exception. (str "Ran out of decks. Check :first-deck-num in config."))))

            initial-game-state (assoc initial-game-map :deck-number (:first-deck-num config))]
        (reset! game-state-atom initial-game-state)))
    ;; The main thread ends here, but the UI thread is alive.
    ;; System/exit will be called by the UI's "Exit" button or window close.


    ;; --- PATH 2: BATCH MODE ---
    (do
      (println (str "Starting in batch mode for " (:num-of-decks config) " decks..."))
      (let [[_ final-results]
            (loop [deck-number (:first-deck-num config)
                   record-of-results {:lost-limit-reached 0 :lost-field-repeated 0 :won 0}] ; [cite: 71]
              (if (< deck-number (+ (:first-deck-num config) (:num-of-decks config)))
                (let [initial-game-map (game/deal-next-deck) ; [cite: 72]

                      ;; --- THIS IS "SOLUTION 2" FOR BATCH ---
                      _ (when (nil? initial-game-map)
                          (throw (Exception. (str "Ran out of decks. Deck limit: " (:num-of-decks config)))))

                      initial-game-state (assoc initial-game-map :deck-number deck-number)
                      _ (reset! game-state-atom initial-game-state)

                      ;; Call the non-interactive, fast "play-game"
                      result (game/play-game)

                      updated-results
                      (cond
                        (= (:result result) :won)
                        (update record-of-results :won inc) ; [cite: 74]

                        (= (:result result) :lost-limit-reached)
                        (update record-of-results :lost-limit-reached inc) ; [cite: 75]

                        (= (:result result) :lost-field-repeated)
                        (update record-of-results :lost-field-repeated inc) ; [cite: 75-76]

                        ;; :quit-by-user and :exit-program can't happen in batch mode
                        ;; but this is fine.
                        (= (:result result) :quit-by-user)
                        (update record-of-results :quit-by-user (fnil inc 0)) ; [cite: 77]

                        (= (:result result) :exit-program)
                        record-of-results ; [cite: 77]

                        :else
                        (do
                          (println "Unexpected result:" result) ; [cite: 78]
                          record-of-results))] ; [cite: 78]

                  (when (= (:result result) :won)
                    (spit "decks-won-clojure.txt" (str "\nDeck number " deck-number " won.") :append true)) ; [cite: 78]

                  (if (= (:result result) :exit-program)
                    [deck-number updated-results]
                    (recur (inc deck-number) updated-results))) ; [cite: 78-79]

                [deck-number record-of-results]))] ; [cite: 79]

        (println "Record of Results:" final-results) ; [cite: 79]
        (System/exit 0)) ; [cite: 79]
      )))