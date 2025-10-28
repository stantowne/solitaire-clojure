(ns solitaire-clojure.ui
  (:require [cljfx.api :as fx]
            [solitaire-clojure.state :refer [game-state-atom]]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(def card-width 75)
(def card-height 115)
(def card-v-overlap 30)
(def card-h-overlap 20)

;; This map translates your suit numbers to their file letters
(def suit->char
  {0 "C"
   1 "D"
   2 "S"
   3 "H"})

(defn card->image-path
  "Translates a card map into its corresponding image file path."
  [card]
  (if-let [{:keys [suit value face-up]} card]
    ;; It's a card
    (if face-up
      ;; Card is face up, build the filename
      (let [val-str (str/replace (format "%2d" value) " " "0"); Formats 1 -> "01"
            suit-str (suit->char suit)]
        (str "images/cards/std/" val-str suit-str ".png"))

      ;; Card is face down
      "images/cards/std/00.png")

    ;; It's not a card (it's nil), so it's an empty spot
    nil)) ; We'll handle 'nil' by just not drawing anything
    ;
(defn card-view
  "Returns a cljfx component for a single card or an empty slot."
  [card]
  (if-let [image-path (card->image-path card)]
    ;; Case 1: We have a card, show the image and SCALE IT
    {:fx/type :image-view
     :fit-width card-width
     :fit-height card-height
     :image {:fx/type :image

             ;; --- THIS IS THE FIX ---
             ;; Just pass the string path, not the URL object
             :url image-path}}

    ;; Case 2: No card (nil), show the empty slot placeholder
    {:fx/type :rectangle
     :width card-width
     :height card-height
     :fill "#00000011"
     :stroke :lightgray
     :arc-width 10
     :arc-height 10}))

(defn waste-pile-view
  "Returns a cljfx component for the splayed Waste pile."
  [pile]
  {:fx/type :stack-pane
   :alignment :top-left
   ;; We always include the base slot, then add cards on top
   :children (into [(card-view nil)]
                   (map-indexed
                     (fn [idx card]
                       (assoc (card-view card)
                         ;; Splay horizontally
                         :translate-x (* idx card-h-overlap)))
                     ;; Only show the last 3 cards
                     (take-last 3 pile)))})

(defn tableau-pile-view
  "Returns a cljfx component for a single splayed Tableau pile."
  [pile]
  {:fx/type :stack-pane
   ;; Aligns all children to the top-left of the stack
   :alignment :top-left
   :children (into [(card-view nil)] ; Always start with the empty base slot
                   (map-indexed
                     (fn [idx card]
                       ;; (card-view card) returns the image map.
                       ;; We 'assoc' :translate-y to offset it.
                       (assoc (card-view card)
                         :translate-y (* idx card-v-overlap)))
                     pile))})

;; --- 1. The View (What the UI looks like) ---
;; This function is like a template. It takes the current
;; game state map and describes what UI to show.
(defn root-view
  "Describes the entire UI window"
  [{:keys [deck-number moves-made field]}]
  ;; --- 1. Get :waste from the field ---
  (let [{:keys [stock foundations tableau waste]} field]
    {:fx/type :stage
     :showing true
     :title "Solitaire Monitor"
     :width 650
     :height 900
     :on-close-request (fn [_] (System/exit 0))
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :padding 20
                    :spacing 10
                    :children [
                               ;; ... (labels are unchanged) ...
                               {:fx/type :label
                                :text (str "Deck: " deck-number)}
                               {:fx/type :label
                                :text (str "Moves: " moves-made)}

                               {:fx/type :grid-pane
                                :hgap 10
                                :vgap 20
                                :children (let [
                                                ;; --- ROW 0 ---
                                                stock-pile (assoc (card-view (last stock))
                                                             :grid-pane/column 0
                                                             :grid-pane/row 0)

                                                ;; --- 2. REPLACE waste-slot ---
                                                waste-pile (assoc (waste-pile-view waste)
                                                             :grid-pane/column 1
                                                             :grid-pane/row 0
                                                             :grid-pane/column-span 2)

                                                foundation-piles (map-indexed
                                                                   (fn [idx pile]
                                                                     (assoc (card-view (last pile))
                                                                       :grid-pane/column (+ idx 3)
                                                                       :grid-pane/row 0))
                                                                   foundations)

                                                ;; --- ROW 1 ---
                                                tableau-piles (map-indexed
                                                                (fn [idx pile]
                                                                  (assoc (tableau-pile-view pile)
                                                                    :grid-pane/column idx
                                                                    :grid-pane/row 1))
                                                                tableau)]

                                            ;; --- 3. UPDATE concat list ---
                                            (vec (concat [stock-pile waste-pile] ;<-- Use waste-pile
                                                         foundation-piles
                                                         tableau-piles)))}
                               ]}}}))
;; --- 2. The Renderer (The cljfx "engine") ---
;; This creates the cljfx system that knows how to
;; "render" your view.
(def renderer
  (fx/create-renderer
    ;; We tell the renderer to use our root-view function
    ;; as its main template.
    :middleware (fx/wrap-map-desc root-view)))

;; --- 3. The Launcher (The "start" function) ---
;; This is the one function we will call from core.clj.
(defn launch-ui []
  ;; This is the magic line. It tells cljfx to:
  ;; 1. "mount" the renderer.
  ;; 2. "watch" the game-state-atom.
  ;; 3. Anytime game-state-atom changes, automatically
  ;;    re-run our root-view function with the new data.
  (fx/mount-renderer game-state-atom renderer))
