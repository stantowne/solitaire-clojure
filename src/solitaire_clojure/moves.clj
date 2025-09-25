(ns solitaire-clojure.moves
  (:require [solitaire-clojure.helper-functions
             :refer [force-card-face-up force-last-card-pile-face-up force-card-face-down dif-color]]))
(defn flip
  "'Flips' the stock to the waste or the waste to the stock according to Klondike rules"
  [game-state]
  (let [stock (:field (:stock game-state))
        waste (:field (:waste game-state))
        stock-count (count stock)
        waste-count (count waste)]

    (cond
      (and (= stock-count 0) (= waste-count 0)) ;; no cards in stock or waste
      game-state ;; no change

      (= stock-count 0) ;; no cards in stock
      (let [new-stock (mapv force-card-face-down (vec (reverse waste)))
            new-waste []
            new-field (assoc (:field game-state) :stock new-stock :waste new-waste)
            new-moves-made (inc (:moves-made game-state))]
        (assoc game-state :field new-field :moves-made new-moves-made))

      (> stock-count 2) ;; three or more cards in stock
      (let [new-stock (subvec stock 0 (- stock-count 3))
            new-waste (vec (concat waste (map force-card-face-up (reverse (subvec stock (- stock-count 3) stock-count)))))
            new-field (assoc (:field game-state) :stock new-stock :waste new-waste)
            new-moves-made (inc (:moves-made game-state))]
        (assoc game-state :field new-field :moves-made new-moves-made))

      :else ;; only 1 or 2 cards in stock
      (let [new-stock []
            new-waste (vec (concat waste (map force-card-face-up (reverse (stock)))))
            new-field (assoc (:field game-state) :stock new-stock :waste new-waste)]
        (assoc game-state :stock new-stock :waste new-waste)))))

(defn move-a-card-from-waste-to-foundation
  "move the last card in the waste to the foundation in certain cases; otherwise return nil"
  [game-state max-value-to-move]
  (let [waste (:field (:waste game-state))]
    (if (empty? waste)
      nil
      (let [waste-last-card (last waste)
            suit-number (:suit waste-last-card)
            value (:value waste-last-card)
            foundation-value (get (:field (:foundations game-state)) suit-number)]
        (if (and (not (> value max-value-to-move)) (= value (inc foundation-value)))
          (let [new-foundations (update (:field (:foundations game-state)) suit-number inc)
                new-waste (vec (butlast waste))
                new-field (assoc (:field game-state) :foundations new-foundations :waste new-waste)
                new-moves-made (inc (:moves-made game-state))]
            {assoc game-state :field new-field :moves-made new-moves-made})
          nil)))))

(defn move-a-card-from-pile-to-foundation
  "move the last card in a pile to the foundation in certain cases; otherwise return nil"
  [game-state max-value-to-move]
  (for [pile-num (range 7)]
    (let [pile (get (:tableau (:field game-state)) pile-num)]
      (if (empty? pile)
        nil
        (let [pile-last-card (last pile)
              suit-number (:suit pile-last-card)
              value (:value pile-last-card)
              foundation-value (get (:field (:foundations game-state)) suit-number)]
          (if (and (not (> value max-value-to-move)) (= value (inc foundation-value)))
            (let [new-foundations (update (:field (:foundations game-state)) suit-number inc)
                  new-pile (force-last-card-pile-face-up (vec (butlast pile)))
                  new-tableau (assoc (:tableau (:field game-state)) pile-num new-pile)
                  new-field (assoc (:field game-state) :foundations new-foundations :tableau new-tableau)
                  new-moves-made (inc (:moves-made game-state))]
              {assoc game-state :field new-field :moves-made new-moves-made})
            nil))))))

(defn move-a-card-from-waste-to-pile
  "move the last card in the waste to a tableau pile in certain cases; otherwise return"
  [game-state]
  (let [waste (:field (:waste game-state))]
    (if (empty? waste)
      nil
      (for [pile-num (range 7)]
         (let [pile (get (:tableau (:field game-state)) pile-num)
               waste-last-card (last waste)]
          (cond
            ;; move king to empty pile
            (and (empty? pile) (= (:value waste-last-card) 13))
            (let [new-pile (vec (conj pile waste-last-card)) ; no need to force face up, waste cards are always face up
                  new-tableau (assoc (:tableau (:field game-state)) pile-num new-pile)
                  new-waste (vec (butlast waste))
                  new-field (assoc (:field game-state) :tableau new-tableau :waste new-waste)
                  new-moves-made (inc (:moves-made game-state))]
              {assoc game-state :field new-field :moves-made new-moves-made})

            ;; empty pile but last waste card isn't a king
            (empty? pile)
            nil

            ;; non-empty pile (regular case)
            :else
            (let [pile-last-card (last pile)]
              (if (and (:face-up pile-last-card) ;; last card in pile must be face up
                       (dif-color pile-last-card waste-last-card)
                       (= (:value pile-last-card) (inc (:value waste-last-card))))
                (let [new-pile (vec (conj pile waste-last-card))
                      new-tableau (assoc (:tableau (:field game-state)) pile-num new-pile)
                      new-waste (vec (butlast waste))
                      new-field (assoc (:field game-state) :tableau new-tableau :waste new-waste)
                      new-moves-made (inc (:moves-made game-state))]
                  {assoc game-state :field new-field :moves-made new-moves-made})
                nil))))))))

(defn move-full-pile-to-different-pile
  "move a full pile to a different tableau pile in certain cases; otherwise return nil"
  [game-state]
  (for [from-pile-num (range 7)]
    (let [from-pile (get (:tableau (:field game-state)) from-pile-num)
          from-pile-up-cards (filter :face-up from-pile)
          from-pile-dn-cards (filter (complement :face-up) from-pile)]
      (if (empty? from-pile)
        nil
        (for [to-pile-num (range 7)]
          (if (= from-pile-num to-pile-num)
            nil
            (let [to-pile (get (:tableau (:field game-state)) to-pile-num)
                  to-pile-last-card (last to-pile)]
              (cond
                ;; move king to empty pile
                (and (empty? to-pile) (= (:value (first from-pile-up-cards) 13))
                (let [new-from-pile (force-last-card-pile-face-up (vec (from-pile-dn-cards)))
                      new-to-pile (vec (into to-pile from-pile-up-cards)) ; no need to force face up, moving card is already face up
                      new-tableau (-> (:tableau (:field game-state))
                                      (assoc from-pile-num new-from-pile)
                                      (assoc to-pile-num new-to-pile))
                      new-field (assoc (:field game-state) :tableau new-tableau)
                      new-moves-made (inc (:moves-made game-state))]
                  {assoc game-state :field new-field :moves-made new-moves-made})

                ;; empty pile but last waste card isn't a king
                (empty? to-pile)
                nil

                ;; non-empty pile (regular case)
                :else
                (let [to-pile-last-card (last to-pile)]
                  (if (and (:face-up to-pile-last-card) ;; last card in pile must be face up
                           (dif-color to-pile-last-card from-pile-last-card)
                           (= (:value to-pile-last-card) (inc (:value from-pile-last-card))))
                    (let [new-from-pile (force-last-card-pile-face-up (vec (butlast from-pile)))
                          new-to-pile (vec (conj to-pile from-pile-last-card))
                          new-tableau (-> (:tableau (:field game-state))
                                          (assoc from-pile-num