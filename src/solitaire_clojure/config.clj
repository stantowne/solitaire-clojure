(ns solitaire-clojure.config)



(def config
  {:decks-filepath "test/resources/decks-made-2022-01-15-count-10000-dict.csv"
   :first-deck-num 0 ; files are treated as 0 base
   :num-of-decks 10000 ;; do not exceed number of decks minus first-deck-num
   :move-limit 200
   :print-each-move false
   :print-func-failure false})

