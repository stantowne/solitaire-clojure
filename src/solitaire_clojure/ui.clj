(ns solitaire-clojure.ui
  (:require [cljfx.api :as fx]
            [solitaire-clojure.state :refer [game-state-atom]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [solitaire-clojure.logic :as game]
            [solitaire-clojure.config :refer [config]]
            [solitaire-clojure.print :refer [print-game-state]]))

(def card-width 75)
(def card-height 115)
(def card-v-overlap 30)
(def card-h-overlap 20)

(def suit->char
  {0 "C" 1 "D" 2 "S" 3 "H"})

(defn card->image-path
  [card]
  (if-let [{:keys [suit value face-up]} card]
    (if face-up
      (let [val-str (str/replace (format "%2d" value) " " "0")
            suit-str (suit->char suit)]
        (str "images/cards/std/" val-str suit-str ".png"))
      "images/cards/std/00.png")
    nil))

(defn card-view
  [card]
  (if-let [image-path (card->image-path card)]
    {:fx/type :image-view
     :fit-width card-width
     :fit-height card-height
     :image {:fx/type :image :url image-path}}
    {:fx/type :rectangle
     :width card-width
     :height card-height
     :fill "#00000011"
     :stroke :lightgray
     :arc-width 10
     :arc-height 10}))

(defn waste-pile-view
  [pile]
  {:fx/type :stack-pane
   :alignment :top-left
   :children (into [(card-view nil)]
                   (map-indexed
                     (fn [idx card]
                       (assoc (card-view card)
                         :translate-x (* idx card-h-overlap)))
                     (take-last 3 pile)))})

(defn tableau-pile-view
  [pile]
  {:fx/type :stack-pane
   :alignment :top-left
   :children (into [(card-view nil)]
                   (map-indexed
                     (fn [idx card]
                       (assoc (card-view card)
                         :translate-y (* idx card-v-overlap)))
                     pile))})

;; --- NEW: A HELPER FOR STYLING BUTTONS ---
(defn button-view [text event disabled?]
  {:fx/type :button
   :text text
   :pref-width 100
   :on-action event
   :disable disabled?})

;; --- NEW: THE MAIN EVENT HANDLER ---
(defn- event-handler [event]
  (case (:event/type event)
    :next-move
    (swap! game-state-atom game/calculate-next-state) ;

    :back-one-move
    (swap! game-state-atom game/back-one-move) ;

    ;; --- THIS LOGIC IS NEW ---
    :next-deck ; (Renamed from :give-up)
    (let [current-deck-num (:deck-number @game-state-atom) ;
          next-deck-num (inc current-deck-num)
          deck-limit (+ (:first-deck-num config) (:num-of-decks config))] ;
      (if (< next-deck-num deck-limit)
        ;; We have more decks to play
        (let [next-deck (game/deal-next-deck)] ;
          (if next-deck
            (reset! game-state-atom (assoc next-deck :deck-number next-deck-num)) ;
            ;; Ran out of decks in the CSV file
            (swap! game-state-atom assoc :game-result :no-more-decks))) ;
        ;; Reached the configured deck limit
        (swap! game-state-atom assoc :game-result :no-more-decks))) ;

    :exit-program
    (System/exit 0)) ;

  (when (:print-each-move? config)
    (print-game-state @game-state-atom)))




;; --- UPDATED: THE ROOT LAYOUT ---
(defn root-view
  "Describes the entire UI window"
  [{:keys [deck-number moves-made field game-result seen-fields]}]
  (let [{:keys [stock foundations tableau waste]} field
        game-is-over? (not= :in-progress game-result)
        no-more-decks? (= :no-more-decks game-result)
        cant-go-back? (< (count seen-fields) 2)
        status-text (case game-result
                      :won "Game Won"
                      :lost-limit-reached "Game Lost (limit reached)"
                      :lost-field-repeated "Game Lost (field repeated)"
                      :lost-no-moves-possible "Game Lost (no moves possible)"
                      :no-more-decks "No More Decks"
                      "")] ; Default to an empty string
    {:fx/type :stage
     :showing true
     :title "Klondike Solitaire in Clojure"
     :width 800
     :height 900
     :on-close-request (fn [_] (System/exit 0))
     :scene {:fx/type :scene
             :root {:fx/type :h-box
                    :padding 20
                    :spacing 10
                    :children [
                               {:fx/type :v-box
                                :spacing 10
                                :pref-width 100
                                :children (->> [
                                                {:fx/type :label
                                                 :text (str "Deck: " deck-number)} ;
                                                {:fx/type :label
                                                 :text (str "Moves: " moves-made)} ;

                                                (when game-is-over?
                                                  {:fx/type :label
                                                   :style {:-fx-font-weight :bold
                                                           :-fx-text-fill :red}
                                                   :text status-text
                                                   :wrap-text true})


                                                (button-view "Next Move" {:event/type :next-move} game-is-over?) ;
                                                (button-view "Back" {:event/type :back-one-move} cant-go-back?) ;
                                                (button-view "Next Deck" {:event/type :next-deck} no-more-decks?) ;
                                                (button-view "Exit" {:event/type :exit-program} false)] ;
                                               (remove nil?)
                                               (vec))}

                               {:fx/type :grid-pane ;
                                :hgap 10
                                :vgap 20
                                :children (let [
                                                ;; --- 2. STOCK PILE UPDATED ---
                                                stock-pile (assoc {:fx/type :v-box
                                                                   :alignment :center
                                                                   :spacing 5
                                                                   :children [{:fx/type :label :text (str (count stock))}
                                                                              (card-view (last stock))]} ;
                                                             :grid-pane/column 0 :grid-pane/row 0)

                                                ;; --- 3. WASTE PILE UPDATED ---
                                                waste-pile (assoc {:fx/type :v-box
                                                                   :alignment :center
                                                                   :spacing 5
                                                                   :children [{:fx/type :label :text (str (count waste))}
                                                                              (waste-pile-view waste)]} ;
                                                             :grid-pane/column 1 :grid-pane/row 0
                                                             :grid-pane/column-span 2)

                                                foundation-piles (map-indexed
                                                                   (fn [idx pile]
                                                                     (assoc (card-view (last pile))
                                                                       :grid-pane/column (+ idx 3) :grid-pane/row 0)) ;
                                                                   foundations)
                                                tableau-piles (map-indexed
                                                                (fn [idx pile]
                                                                  (assoc (tableau-pile-view pile)
                                                                    :grid-pane/column idx :grid-pane/row 1)) ;
                                                                tableau)]
                                            (vec (concat [stock-pile waste-pile]
                                                         foundation-piles
                                                         tableau-piles)))} ;
                               ]}}}))

;; --- UPDATED: THE RENDERER (to handle events) ---
(def renderer
  (delay
    (fx/create-renderer
      ;; 1. The :middleware tells cljfx to use root-view as the blueprint
      :middleware (fx/wrap-map-desc root-view)

      ;; 2. The :opts map is where you tell cljfx which function handles events
      :opts {:fx.opt/map-event-handler event-handler})))

;; --- UPDATED: THE LAUNCHER (to use new renderer) ---
(defn launch-ui []
  (fx/mount-renderer game-state-atom @renderer))