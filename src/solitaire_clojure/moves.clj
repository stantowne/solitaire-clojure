(ns solitaire-clojure.moves
  (:require [solitaire-clojure.helper-functions :as helper]))


(defn move-a-card-from-waste-to-foundations
  "returns a new map with the last card in the waste moved to the foundations in certain cases;
  otherwise returns nil"
  [game-state max-value-to-move]
  (let [{:keys [field moves-made seen-fields]} game-state
        waste (:waste field)
        foundations (:foundations field)]
    (if (empty? waste)
      nil
      (let [waste-last-card (last waste)
            suit-number (:suit waste-last-card)
            value (:value waste-last-card)
            foundation-value (:value (last (foundations suit-number)))]
        (if (and (not (> value max-value-to-move)) (= value ((fnil inc 0) foundation-value)))
          (let [new-foundation (conj (nth foundations suit-number) waste-last-card)
                new-foundations (assoc foundations suit-number new-foundation)
                new-waste (vec (butlast waste))
                new-field (assoc field :foundations new-foundations :waste new-waste)
                new-moves-made (inc moves-made)
                new-seen-fields (conj seen-fields new-field)]
            (assoc game-state :field new-field :moves-made new-moves-made :seen-fields new-seen-fields))
          nil)))))

(defn move-a-card-from-pile-to-foundations
  "returns a new map with the last card in a pile moved to the foundations in certain cases;
  otherwise return nil"
  [game-state do-not-move-above no-final-test-needed-below]
  (some (fn [pile-num]
          (let [{:keys [field moves-made seen-fields]} game-state
                tableau (:tableau field)
                foundations (:foundations field)
                pile (tableau pile-num)]
             (when (seq pile)
               (let [pile-last-card (last pile)
                     suit-number (:suit pile-last-card)
                     value (:value pile-last-card)
                     foundation-value (:value (last (foundations suit-number)))]
                  (when (and (= value ((fnil inc 0) foundation-value))
                             (<= value do-not-move-above)
                             (or
                                (< value no-final-test-needed-below)

                                (and (>= (or (:value (last (foundations (mod (+ suit-number 1) 4)))) 0) (- value 2))
                                     (helper/card-in-tableau-face-up? tableau {:suit (mod (+ suit-number 3) 4) :value (- value 1) :face-up true}))

                                (and (>= (or (:value (last (foundations (mod (+ suit-number 3) 4)))) 0) (- value 2))
                                     (helper/card-in-tableau-face-up? tableau {:suit (mod (+ suit-number 1) 4) :value (- value 1) :face-up true}))

                                (and (>= (or (:value (last (foundations (mod (+ suit-number 1) 4)))) 0) (- value 2))
                                     (>= (or (:value (last (foundations (mod (+ suit-number 3) 4)))) 0) (- value 2)))))

                    (let [new-foundation (conj (nth foundations suit-number) pile-last-card)
                          new-foundations (assoc foundations suit-number new-foundation)
                          new-pile (vec (butlast pile))
                          new-pile (if (seq new-pile)
                                     (helper/force-last-card-pile-face-up new-pile)
                                     new-pile)
                          new-tableau (assoc tableau pile-num new-pile)
                          new-field (assoc field :foundations new-foundations :tableau new-tableau)
                          new-moves-made (inc moves-made)
                          new-seen-fields (conj seen-fields new-field)]
                      (assoc game-state :field new-field :moves-made new-moves-made :seen-fields new-seen-fields)))))))
    (range 7)))

(defn move-a-card-from-waste-to-pile
  "returns a new map with the last card in the waste moved to a tableau pile in certain cases;
  otherwise return"
  [game-state]
  (let [{:keys [field moves-made seen-fields]} game-state
        waste (:waste field)
        waste-last-card (last waste)
        tableau (:tableau field)]
    (if (empty? waste)
      nil
      (some
        (fn [pile-num]
          (let [pile (tableau pile-num)]
            (cond
              ;; move king to empty pile
              (do
                ;; (println "Testing empty pile for king move" pile-num)
                ;; (println "Waste last card:" waste-last-card "Value:" (:value waste-last-card))
                ;; (println "Pile Number:" pile-num "Pile:" pile)
                (and (empty? pile) (= (:value waste-last-card) 13)))
              (let [new-pile (vec (conj pile waste-last-card)) ; no need to force face up, waste cards are always face up
                    new-tableau (assoc tableau pile-num new-pile)
                    new-waste (vec (butlast waste))
                    new-field (assoc field :tableau new-tableau :waste new-waste)
                    new-moves-made (inc moves-made)
                    new-seen-fields (conj seen-fields new-field)]
                (assoc game-state :field new-field :moves-made new-moves-made :seen-fields new-seen-fields))

              ;; empty pile but last waste card isn't a king
              (empty? pile)
              nil

              ;; non-empty pile (regular case)
              :else
              (let [pile-last-card (last pile)]
                (if (and (:face-up pile-last-card) ;; last card in pile must be face up (probably unnecessary)
                         (helper/dif-color? pile-last-card waste-last-card)
                         (= (:value pile-last-card) (inc (:value waste-last-card))))
                  (let [new-pile (vec (conj pile waste-last-card))
                        new-tableau (assoc tableau pile-num new-pile)
                        new-waste (vec (butlast waste))
                        new-field (assoc field :tableau new-tableau :waste new-waste)
                        new-moves-made (inc (:moves-made game-state))
                        new-seen-fields (conj seen-fields new-field)]
                    (assoc game-state :field new-field :moves-made new-moves-made :seen-fields new-seen-fields)))))))
        (range 7)))))

