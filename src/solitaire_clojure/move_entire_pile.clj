(ns solitaire-clojure.move-entire-pile
  (:require [solitaire-clojure.helper-functions :refer [dif-color? sister-card? force-last-card-pile-face-up]]
            [solitaire-clojure.types :as t])
  (:import [solitaire_clojure.types Card Field GameState]))


(defn sister-card-in-tableau? [^Card card ^clojure.lang.IPersistentVector tableau]
  (some
    (fn [pile]
      (some
        (fn [^Card c]
          (and (.-face-up c) (sister-card? c card)))
        pile))
    tableau))

(defn tableau-king-ready-to-move?
  "Returns true if there is a pile (not from-pile-num)
   whose first card is face-down and whose first face-up card is a king."
  [^clojure.lang.IPersistentVector tableau ^long from-pile-num]
  (some
    (fn [^long pile-num] ; Asks is there a pile-num (0 through 6 inclusive) such that:
      (let [^clojure.lang.IPersistentVector pile (nth tableau pile-num)
            ^Card first-card (first pile)
            up-cards (filter ( fn [^Card c] (.-face-up c)) pile)
            ^Card first-up-card (first up-cards)]
        (and (not (= pile-num from-pile-num)) ; (1) pile-num is not from-pile-num
             (not (empty? pile)) ; (2) pile is not empty
             (not (.-face-up first-card)) ; (3) first card is face-down
             first-up-card ; (4) there is at least one face-up card
             (= 13 (.-value first-up-card)))))
    (range 7)))

(defn good-moves [legal-moves ^clojure.lang.IPersistentVector tableau ^clojure.lang.IPersistentVector waste]
  (let [^Card last-waste (last waste)]
    (if (and last-waste (= 13 (.-value last-waste)))
      legal-moves
      (filter
        (fn [move]
          (let [{:keys [from-pile-num from-pile to-pile]} move]
          (or
            ;; (a) from-pile has at least one down card
            (do ;; (println "first test" (some #(not (.-face-up %)) from-pile))
                (some (fn [^Card c] (not (.-face-up c))) from-pile))
            ;; (b) sister card of last to-pile card is face-up in tableau
            (do ;; (println "second test" (sister-card-in-tableau? (last to-pile) tableau))
                (sister-card-in-tableau? ^Card (last to-pile) tableau))
            ;; (c) tableau includes a pile with at least one down card and first-up-card is king
            (do ;; (println "third-test" (tableau-king-ready-to-move? tableau from-pile-num))
                (tableau-king-ready-to-move? tableau from-pile-num)))))
        legal-moves))))

(defn legal-moves [^clojure.lang.IPersistentVector tableau]
  (for [from-pile-num (range 7)
        to-pile-num (range 7)
        :let [^clojure.lang.IPersistentVector from-pile (nth tableau from-pile-num)
              ^clojure.lang.IPersistentVector from-pile-up-cards (filter (fn [^Card c] (.-face-up c)) from-pile)
              ^clojure.lang.IPersistentVector from-pile-dn-cards (remove (fn [^Card c] (.-face-up c)) from-pile)
              ^clojure.lang.IPersistentVector to-pile (nth tableau to-pile-num)]
        :when (and (not (empty? from-pile))
                   (or
                     (and (= (.-value ^Card (first from-pile-up-cards)) 13)
                          (not (empty? from-pile-dn-cards))
                          (empty? to-pile))
                     (and (not (empty? to-pile))
                          (= (.-value ^Card (last to-pile)) (inc (.-value ^Card (first from-pile-up-cards))))
                          (dif-color? (first from-pile-up-cards) (last to-pile)))))]
    {:from-pile-num from-pile-num
     :from-pile from-pile
     :to-pile-num to-pile-num
     :to-pile to-pile}))


(defn move-entire-pile
  "returns a new map with all the face-up cards of a tableau pile moved to a different tableau pile,
  in certain cases; otherwise return nil"
  [^GameState game-state]
  (let [^Field field (.-field game-state)
        moves-made (.-moves-made game-state)
        seen-fields (.-seen-fields game-state)
        tableau (.-tableau field)
        waste (.-waste field)
        legal-moves (legal-moves tableau)
        good-moves (good-moves legal-moves tableau waste)
        best-move (first
                    (sort-by
                      (juxt (fn [move] (- (count (remove (fn [^Card c] (.-face-up c)) (:from-pile move))))) :pile-num)
                      good-moves))]
    ;; (println "legal moves:" legal-moves)
    ;; (println "good moves:" good-moves)
    ;; (println "best move:" best-move)
    (if (nil? best-move)
      nil
      (let [from-pile (:from-pile best-move)
            to-pile (:to-pile best-move)
            from-pile-num (:from-pile-num best-move)
            to-pile-num (:to-pile-num best-move)
            from-pile-up-cards (filter (fn [^Card c] (.-face-up c)) from-pile)
            from-pile-dn-cards (remove (fn [^Card c] (.-face-up c)) from-pile)
            new-from-pile (force-last-card-pile-face-up (vec from-pile-dn-cards))
            new-to-pile (vec (concat to-pile from-pile-up-cards))
            new-tableau (assoc tableau from-pile-num new-from-pile to-pile-num new-to-pile)
            new-field (assoc field :tableau new-tableau)
            new-moves-made (inc moves-made)
            new-seen-fields (conj seen-fields new-field)]
        (assoc game-state :field new-field :moves-made new-moves-made :seen-fields new-seen-fields)))))
