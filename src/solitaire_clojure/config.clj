(ns solitaire-clojure.config)



(def config
  {:decks-filepath     "test/resources/decks-made-2022-01-15-count-10000-dict.csv"
   :first-deck-num     1 ; files are treated as 0 base
   :num-of-decks       1 ;; do not exceed number of decks in file minus first-deck-num
   :move-limit         200
   :print-each-move?   true
   :interactive-mode?  true})

