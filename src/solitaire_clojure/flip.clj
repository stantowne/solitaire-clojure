(ns solitaire-clojure.flip
  (:require [solitaire-clojure.helper-functions :refer [force-card-face-up force-card-face-down]])
  (:require [solitaire-clojure.config :refer [config]]))



(defn flip-three-forward
  "flips three cards from stock to waste"
  [game-state]
  (let [{:keys [field moves-made]} game-state
        waste (:waste field)
        stock (:stock field)
        stock-count (count stock)
        new-stock (subvec stock 0 (- stock-count 3))
        new-waste (vec (concat waste (map force-card-face-up (reverse (subvec stock (- stock-count 3) stock-count)))))
        new-field (assoc field :stock new-stock :waste new-waste)
        new-moves-made (inc moves-made)]
    ;; (println "leaving flip-three-forward")
    ;; (println "stock count: " (count (:stock (:field game-state))))
    ;; (println "waste count: " (count (:waste (:field game-state))))
    ;; (println "new stock count: " (count new-stock))
    ;; (println "new waste count: " (count new-waste))
    (do
      (println "flip-three-forward")
      (assoc game-state :field new-field :moves-made new-moves-made))))



(defn flip
  "returns a new map with three (or fewer) cards flipped from stock to waste or the entire waste flipped to stock;
  if both stock and waste are empty returns the same map as was passed in
  this is the only function which never returns nil
  it is the innermost function in the if-let group"
  [game-state]
  (let [{:keys [field moves-made]} game-state
        waste (:waste field)
        stock (:stock field)
        initial-flips-of-round (:initial-flips-of-round field)
        stock-count (count stock)
        waste-count (count waste)]

    (cond
      (and (= stock-count 0) (= waste-count 0))
      (let [new-moves-made (inc moves-made)]
        (assoc game-state :moves-made new-moves-made))

      (= stock-count 0) ;; no cards in stock -- flip entire waste to stock
      (let [new-stock (mapv force-card-face-down (vec (reverse waste)))
            new-waste []
            flip-times (max 0 (min (- initial-flips-of-round 1) (- (quot (count new-stock) 3) 1))) ;; how many times to flip three forward
            new-field (assoc field :stock new-stock :waste new-waste :initial-flips-of-round flip-times)
            new-moves-made (inc moves-made)
            new-game-state (assoc game-state :field new-field :moves-made new-moves-made )]
        ;; (println "stock-count: " stock-count)
        ;; (println "stock: " stock)
        ;; (println "waste: " waste)
        ;; (println "new-stock: " new-stock)
        ;; (println "new-waste: " new-waste)
        ;; (println "flip-times: " flip-times)
        (nth (iterate flip-three-forward new-game-state) flip-times)) ;; now flip three forward

      (> stock-count 2) ;; three or more cards in stock
      (flip-three-forward game-state)

      :else ;; only 1 or 2 cards in stock
      (let [new-stock []
            new-waste (vec (concat waste (map force-card-face-up (reverse stock))))
            new-field (assoc field :stock new-stock :waste new-waste)
            new-moves-made (inc moves-made)]
        (assoc game-state :field new-field :moves-made new-moves-made)))))
