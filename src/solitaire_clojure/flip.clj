(ns solitaire-clojure.flip
  (:require [solitaire-clojure.helper-functions
             :refer [force-card-face-up force-card-face-down]]))


(defn flip
  "returns a new map with three (or fewer) cards flipped from stock to waste or the entire waste flipped to stock;
  if both stock and waste are empty returns the same map as was passed in
  this is the only function which never returns nil
  it is the innermost function in the if-let group"
  [game-state]
  (let [{:keys [field moves-made]} game-state
        waste (:waste field)
        stock (:stock field)
        stock-count (count stock)
        waste-count (count waste)]

    (cond
      (and (= stock-count 0) (= waste-count 0))
      (let [new-moves-made (inc moves-made)]
        (assoc game-state :moves-made new-moves-made))

      (= stock-count 0) ;; no cards in stock
      (let [new-stock (mapv force-card-face-down (vec (reverse waste)))
            new-waste []
            new-field (assoc field :stock new-stock :waste new-waste)
            new-moves-made (inc moves-made)]
        (assoc game-state :field new-field :moves-made new-moves-made))

      (> stock-count 2) ;; three or more cards in stock
      (let [new-stock (subvec stock 0 (- stock-count 3))
            new-waste (vec (concat waste (map force-card-face-up (reverse (subvec stock (- stock-count 3) stock-count)))))
            new-field (assoc field :stock new-stock :waste new-waste)
            new-moves-made (inc moves-made)]
        (assoc game-state :field new-field :moves-made new-moves-made))

      :else ;; only 1 or 2 cards in stock
      (let [new-stock []
            new-waste (vec (concat waste (map force-card-face-up (reverse stock))))
            new-field (assoc field :stock new-stock :waste new-waste)
            new-moves-made (inc moves-made)]
        (assoc game-state :field new-field :moves-made new-moves-made)))))
