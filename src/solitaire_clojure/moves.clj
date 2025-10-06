(ns solitaire-clojure.moves
  (:require [solitaire-clojure.helper-functions
             :refer [force-card-face-up force-last-card-pile-face-up force-card-face-down dif-color card-in-tableau-face-up]]))

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

(defn move-a-card-from-waste-to-foundations
  "returns a new map with the last card in the waste moved to the foundations in certain cases;
  otherwise returns nil"
  [game-state max-value-to-move]
  (let [{:keys [field moves-made]} game-state
        waste (:waste field)
        foundations (:foundations field)]
    (if (empty? waste)
      nil
      (let [waste-last-card (last waste)
            suit-number (:suit waste-last-card)
            value (:value waste-last-card)
            foundation-value (foundations suit-number)]
        (if (and (not (> value max-value-to-move)) (= value (inc foundation-value)))
          (let [new-foundations (update foundations suit-number inc)
                new-waste (vec (butlast waste))
                new-field (assoc field :foundations new-foundations :waste new-waste)
                new-moves-made (inc moves-made)]
            (assoc game-state :field new-field :moves-made new-moves-made))
          nil)))))

(defn move-a-card-from-pile-to-foundations
  "returns a new map with the last card in a pile moved to the foundations in certain cases;
  otherwise return nil"
  [game-state max-value-to-auto-move]
  (some (fn [pile-num]
          (let [{:keys [field moves-made]} game-state
                tableau (:tableau field)
                foundations (:foundations field)
                pile (tableau pile-num)]
             (when (seq pile)
               (let [pile-last-card (last pile)
                     suit-number (:suit pile-last-card)
                     value (:value pile-last-card)
                     foundation-value (foundations suit-number)]
                  (when (and (= value (inc foundation-value))
                             (or
                                (<= value max-value-to-auto-move)
                                (and (>= (foundations (mod (+ suit-number 1) 4)) (- foundation-value 2))
                                     (card-in-tableau-face-up tableau {:suit (mod (+ suit-number 3) 4) :value (- value 1)}))
                                (and (>= (foundations (mod (+ suit-number 3) 4)) (- foundation-value 2))
                                     (card-in-tableau-face-up tableau {:suit (mod (+ suit-number 1) 4) :value (- value 1)}))
                                (and (>= (foundations (mod (+ suit-number 1) 4)) (- foundation-value 2))
                                     (>= (foundations (mod (+ suit-number 3) 4)) (- foundation-value 2)))))
                    (let [new-foundations (update foundations suit-number inc)
                          new-pile (vec (butlast pile))
                          new-pile (if (seq new-pile)
                                     (force-last-card-pile-face-up new-pile)
                                     new-pile)
                          new-tableau (assoc tableau pile-num new-pile)
                          new-field (assoc field :foundations new-foundations :tableau new-tableau)
                          new-moves-made (inc moves-made)]
                      (assoc game-state :field new-field :moves-made new-moves-made)))))))
    (range 7)))

(defn move-a-card-from-waste-to-pile
  "returns a new map with the last card in the waste moved to a tableau pile in certain cases;
  otherwise return"
  [game-state]
  (let [{:keys [field moves-made]} game-state
        waste (:waste field)
        tableau (:tableau field)]
    (if (empty? waste)
      nil
      (some
        (fn [pile-num]
         (let [pile (nth tableau pile-num)
               waste-last-card (last waste)]
          (cond
            ;; move king to empty pile
            (and (empty? pile) (= (:value waste-last-card) 13))
            (let [new-pile (vec (conj pile waste-last-card)) ; no need to force face up, waste cards are always face up
                  new-tableau (assoc tableau pile-num new-pile)
                  new-waste (vec (butlast waste))
                  new-field (assoc field :tableau new-tableau :waste new-waste)
                  new-moves-made (inc moves-made)]
              (assoc game-state :field new-field :moves-made new-moves-made))

            ;; empty pile but last waste card isn't a king
            (empty? pile)
            nil

            ;; non-empty pile (regular case)
            :else
            (let [pile-last-card (last pile)]
              (if (and (:face-up pile-last-card) ;; last card in pile must be face up (probably unnecessary)
                       (dif-color pile-last-card waste-last-card)
                       (= (:value pile-last-card) (inc (:value waste-last-card))))
                (let [new-pile (vec (conj pile waste-last-card))
                      new-tableau (assoc tableau pile-num new-pile)
                      new-waste (vec (butlast waste))
                      new-field (assoc field :tableau new-tableau :waste new-waste)
                      new-moves-made (inc (:moves-made game-state))]
                  (assoc game-state :field new-field :moves-made new-moves-made)))))))
        (range 7)))))

