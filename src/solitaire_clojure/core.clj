(ns solitaire-clojure.core
  (:gen-class)
  (:require [solitaire-clojure.config :refer [config]]
            [solitaire-clojure.state :refer [game-state-atom]]
            [solitaire-clojure.logic :as game]))

(defn -main
  "Main entry point for the Solitaire game"
  []
  ;; Initialize the CSV reader once for both modes
  ;; init-csv-reader drops the first N decks as specified in config
  (game/init-csv-reader (:decks-filepath config) (:first-deck-num config))

  (if (:interactive-mode? config)

    ;; --- PATH 1: INTERACTIVE UI MODE ---
    (do
      (println "Starting in interactive (UI) mode...")
      ;; 2.  Deal the first deck and load it into the atom FIRST
      (let [initial-game-map (game/deal-next-deck (:first-deck-num config))
            _ (when (nil? initial-game-map)
                (throw (Exception.
                  (str "Failed to deal next deck; ran out of decks in csv file."))))

            initial-game-state (assoc initial-game-map :deck-number (:first-deck-num config))]
        (reset! game-state-atom initial-game-state))
    ;; 3. Now, load and launch the UI at RUNTIME using requiring-resolve
      (let [launch-fn (requiring-resolve 'solitaire-clojure.ui/launch-ui)]
        (launch-fn))

    ;; 4. Keep the main thread alive so the UI thread can run
      (Thread/sleep Long/MAX_VALUE))


    ;; --- PATH 2: BATCH MODE ---

    (time
      (do
      (println (str "Starting in batch mode for " (:num-of-decks config) " decks starting with deck " (:first-deck-num config) "..."))
      (let [[_ final-results]
            (loop [deck-number (:first-deck-num config)
                   record-of-results {:lost-limit-reached 0
                                      :lost-field-repeated 0
                                      :won 0
                                      :lost-no-moves-possible 0}]
              (if (< deck-number (+ (:first-deck-num config) (:num-of-decks config)))
                (let [initial-game-map (game/deal-next-deck deck-number)

                      _ (when (zero? (mod deck-number 2000))
                          (println (str "Processing Deck: " deck-number "...")))

                      _ (when (nil? initial-game-map)
                          (throw (Exception. (str "Ran out of decks. Deck limit: " (:num-of-decks config)))))

                      _ (reset! game-state-atom initial-game-map) ;load the game-state-map into the atom

                      ;; Call the non-interactive, fast "play-game"
                      result (game/play-game)

                      updated-results
                      (cond
                        (= (:result result) :won)
                        (do
                          (when (:log-deck-by-deck-results? config)
                            (spit "deck-by-deck-results.txt" (str "Deck number " deck-number " won!\n") :append true))
                          (when (:log-wins? config)
                            (spit "decks-won-clojure.txt" (str deck-number "\n") :append true))
                          (update record-of-results :won inc))

                        (= (:result result) :lost-limit-reached)
                        (do
                          (when (:log-deck-by-deck-results? config)
                            (spit "deck-by-deck-results.txt" (str "Deck number " deck-number " lost: limit reached.\n") :append true))
                          (update record-of-results :lost-limit-reached inc))

                        (= (:result result) :lost-field-repeated)
                        (do
                          (when (:log-deck-by-deck-results? config)
                            (spit "deck-by-deck-results.txt" (str "Deck number " deck-number " lost: field repeated.\n") :append true))
                          (update record-of-results :lost-field-repeated inc))

                        (= (:result result) :lost-no-moves-possible)
                        (do
                          (when (:log-deck-by-deck-results? config)
                            (spit "deck-by-deck-results.txt" (str "\nDeck number " deck-number " lost: no moves possible.") :append true))
                          (update record-of-results :lost-no-moves-possible inc))

                        ;; residue from when play-game included move by move control at the command line
                        (= (:result result) :quit-by-user)
                        (update record-of-results :quit-by-user (fnil inc 0))

                        ;; same as above
                        (= (:result result) :exit-program)
                        record-of-results

                        ;; should not happen
                        :else
                        (do
                          (println "Unexpected result:" result)
                          record-of-results))]

                  (if (= (:result result) :exit-program)
                    [deck-number updated-results]
                    (recur (inc deck-number) updated-results)))

                [deck-number record-of-results]))]

        (println "Record of Results:" final-results)
        )
      )))(System/exit 0))

