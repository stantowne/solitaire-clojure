(ns solitaire-clojure.move-partial-pile
  (:require [solitaire-clojure.helper-functions
             :refer [index-of force-last-card-pile-face-up]]))

(declare find-partial-move)

(defn move-partial-pile
  "returns a new map with (1) one or more, but fewer than all, of the face-up cards of a tableau pile moved to a different tableau pile,
  and (2) the immediately preceding face up card from the from-pile moved up to the foundations, in certain cases;
  otherwise return nil"
  [game-state]
  (let [{:keys [field moves-made seen-fields]} game-state
        tableau (:tableau field)
        foundations (:foundations field)
        partial-move (find-partial-move tableau foundations)
        {:keys [from-pile-num to-pile-num card-to-move-up]} partial-move]
    (if (and (some? from-pile-num) (some? to-pile-num) (some? card-to-move-up))
      (let [new-foundation (conj (nth foundations (:suit card-to-move-up)) card-to-move-up)
            new-foundations (assoc foundations (:suit card-to-move-up) new-foundation)
            from-pile (nth tableau from-pile-num)
            new-from-pile (force-last-card-pile-face-up (subvec from-pile 0 (index-of from-pile card-to-move-up)))
            cards-to-move-over (subvec from-pile (inc (index-of from-pile card-to-move-up)))
            to-pile (nth tableau to-pile-num)
            new-to-pile (into to-pile cards-to-move-over)
            new-tableau (assoc tableau from-pile-num new-from-pile to-pile-num new-to-pile)
            new-field (assoc field :tableau new-tableau :foundations new-foundations)
            new-moves-made (inc moves-made)
            new-seen-fields (conj seen-fields new-field)]
        (assoc game-state :field new-field :moves-made new-moves-made :seen-fields new-seen-fields)))))


  (defn find-partial-move [tableau foundations]
    (first
      (for [from-pile-num (range 7) ; 1.  Try each tableau pile as the source.
            :let [from-pile (nth tableau from-pile-num) ; 2.  Get the pile.
                  from-pile-up-cards (filter :face-up from-pile)] ;  3.  Get face-up cards.
            card from-pile-up-cards ; 4.  Try each face-up card.  Note that this is another local binding.
            :when (not= card (last from-pile)) ; 5.  Skip the last card.
            :let [to-pile-num
                  (some (fn [to-pile-num]
                          (let [to-pile (nth tableau to-pile-num)]; 6.  Find a valid destination pile.
                            (when (and (not= from-pile-num to-pile-num)
                                       (not (empty? to-pile))
                                       (= (last to-pile)
                                          {:value (:value card) :suit (mod (+ 2 (:suit card)) 4) :face-up true}))
                              to-pile-num)))
                        (range 7))]
            :when (and to-pile-num  ; 7. Only proceed is a valid destination pile was found.
                       (= (:value card) ((fnil inc 0) (:value (last (nth foundations (:suit card)))))))]
        {:from-pile-num from-pile-num
         :to-pile-num to-pile-num
         :card-to-move-up card})))

