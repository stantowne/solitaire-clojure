(ns solitaire-clojure.move-partial-pile
  (:require [solitaire-clojure.helper-functions
             :refer [force-last-card-pile-face-up dif-color index-of]]))


(defn move-partial-pile
  "returns a new map with (1) one or more, but fewer than all, of the face-up cards of a tableau pile moved to a different tableau pile,
  and (2) the immediately preceding face up card from the from-pile moved up to the foundations, in certain cases;
  otherwise return nil"
  [game-state]
  (let [{:keys [field moves-made]} game-state
        tableau (:tableau field)
        foundations (:foundations field)
        from-pile-num (some (fn [from-pile-num]
                      (let [from-pile (nth tableau from-pile-num)
                            from-pile-up-cards (filter :face-up from-pile)]
                        (when (and (not (empty? from-pile)) ; from pile not empty
                                   (>= (count (from-pile-up-cards)) 2) ; at least two face up cards in from-pile
                                   ;; at least one of the face up cards in from-pile can be placed on a foundation
                                   (some (fn [card] (and (not (= card (last from-pile))) ; not the last card in from-pile
                                                         (= (:value card) (+ 1 (nth foundations (:suit card))))
                                                         (some (fn [to-pile-num] (and (not= from-pile-num to-pile-num)
                                                                                      (= (last (nth tableau to-pile-num))
                                                                                         {:value (:value card) :suit (mod (+ 2 (:suit card)) 4) :face-up true})))
                                                               (range 7))))
                                         from-pile-up-cards)))))
                      (range 7))
        card-to-move-up (some (fn [from-pile-num]
                          (let [from-pile (nth tableau from-pile-num)
                                from-pile-up-cards (filter :face-up from-pile)]
                            (some (fn [card] (and (not (= card (last from-pile))) ; not the last card in from-pile
                                                  (= (:value card) (+ 1 (nth foundations (:suit card))))
                                                  (some (fn [to-pile-num] (and (not= from-pile-num to-pile-num)
                                                                               (= (last (nth tableau to-pile-num))
                                                                                  {:value (:value card) :suit (mod (+ 2 (:suit card)) 4) :face-up true})))
                                                        (range 7))))
                                  from-pile-up-cards)))
        to-pile-num (some  (fn [to-pile-num]
                              (when (not= from-pile-num to-pile-num)
                                (let [to-pile (nth tableau to-pile-num)]
                                  (when (and (not (empty? to-pile))
                                             (= (last to-pile)
                                                {:value (:value card-to-move-up) :suit (mod (+ 2 (:suit card-to-move-up)) 4) :face-up true}))
                                    to-pile-num))))
                            (range 7))]
    (if (and (some? from-pile-num) (some? to-pile-num) (some? card-to-move-up))
      (let [new-foundations (update foundations (:suit card-to-move-up) inc)
            from-pile (nth tableau from-pile-num)
            new-from-pile (subvec from-pile 0 (index-of from-pile card-to-move-up))
            cards-to-move-over (subvec from-pile (+ 1 (index-of from-pile card-to-move-up)))
            to-pile (nth tableau to-pile-num)
            new-to-pile (into to-pile cards-to-move-over)
            new-tableau (assoc tableau from-pile-num new-from-pile to-pile-num new-to-pile)
            new-field (assoc field :tableau new-tableau :foundations new-foundations)
            new-moves-made (inc moves-made)]
        (assoc game-state :field new-field :moves-made new-moves-made)))))
