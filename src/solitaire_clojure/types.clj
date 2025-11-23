(ns solitaire-clojure.types)

(defrecord Card [^long value ^long suit ^boolean face-up])

(defrecord Field [^clojure.lang.IPersistentVector stock
                  ^clojure.lang.IPersistentVector waste
                  ^clojure.lang.IPersistentVector tableau
                  ^clojure.lang.IPersistentVector foundations])

(defrecord GameState [field
                     ^long moves-made
                     seen-fields
                     game-result
                     ^long move-limit
                     ^long deck-number])