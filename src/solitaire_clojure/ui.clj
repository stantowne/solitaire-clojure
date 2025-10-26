(ns solitaire-clojure.ui
  (:require [cljfx.api :as fx]
            [solitaire-clojure.state :refer [game-state-atom]]))

;; --- 1. The View (What the UI looks like) ---
;; This function is like a template. It takes the current
;; game state map and describes what UI to show.
(defn root-view [{:keys [deck-number moves-made]}]
  {:fx/type :stage ; :stage is the main window
   :showing true
   :title "Solitaire Monitor"
   :width 250
   :height 120

   ;; This handles the "X" button on the window.
   ;; It tells the whole program to exit.
   :on-close-request (fn [_] (System/exit 0))

   :scene {:fx/type :scene
           :root {:fx/type :v-box ; A vertical box layout
                  :padding 20
                  :alignment :center-left
                  :spacing 10
                  :children [
                             ;; This label reads the deck number
                             {:fx/type :label
                              ;; We use (str ...) to make sure it's text
                              :text (str "Deck: " deck-number)}

                             ;; This label reads the moves made
                             {:fx/type :label
                              :text (str "Moves Made: " moves-made)}
                             ]}}})

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
