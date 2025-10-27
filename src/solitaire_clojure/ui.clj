(ns solitaire-clojure.ui
  (:require [cljfx.api :as fx]
            [solitaire-clojure.state :refer [game-state-atom]]
            [clojure.java.io :as io]
            [clojure.string :as str]))


(def card-width 75)
(def card-height 115)

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

;; --- 1. The View (What the UI looks like) ---
;; This function is like a template. It takes the current
;; game state map and describes what UI to show.
(defn root-view
  "Describes the entire UI window"
  [{:keys [deck-number moves-made field]}]
  (let [{:keys [stock foundations]} field] ; Get stock and foundations
    {:fx/type :stage
     :showing true
     :title "Solitaire Monitor"
     :width 475 ; Wide enough for 5 piles
     :height 250
     :on-close-request (fn [_] (System/exit 0))
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :padding 20
                    :alignment :top-left
                    :spacing 10
                    :children [
                               {:fx/type :label
                                :text (str "Deck: " deck-number)}

                               {:fx/type :label
                                :text (str "Moves: " moves-made)}

                               ;; --- This is the new H-BOX ---
                               {:fx/type :h-box
                                :spacing 10
                                :children (into [;; 1. The Stock pile
                                                 (card-view (last stock))]

                                                ;; 2. The 4 Foundation piles
                                                (map #(card-view (last %)) foundations))}
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
