(defproject solitaire-clojure "0.1.0-SNAPSHOT"
  :description "Klondike Solitaire Coded in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [
                 [org.clojure/clojure "1.11.1"]
                 [org.clojure/data.csv "1.0.1"]
                 [cljfx "1.9.5"]
                 ;; JavaFX Base (We were missing this)
                 [org.openjfx/javafx-base "21.0.2" :classifier "win"]
                 [org.openjfx/javafx-base "21.0.2" :classifier "linux"]
                 [org.openjfx/javafx-base "21.0.2" :classifier "mac"]
                 [org.openjfx/javafx-base "21.0.2" :classifier "mac-aarch64"] ; For M1/M2

                 ;; JavaFX Graphics
                 [org.openjfx/javafx-graphics "21.0.2" :classifier "win"]
                 [org.openjfx/javafx-graphics "21.0.2" :classifier "linux"]
                 [org.openjfx/javafx-graphics "21.0.2" :classifier "mac"]
                 [org.openjfx/javafx-graphics "21.0.2" :classifier "mac-aarch64"] ; For M1/M2

                 ;; JavaFX Controls
                 [org.openjfx/javafx-controls "21.0.2" :classifier "win"]
                 [org.openjfx/javafx-controls "21.0.2" :classifier "linux"]
                 [org.openjfx/javafx-controls "21.0.2" :classifier "mac"]
                 [org.openjfx/javafx-controls "21.0.2" :classifier "mac-aarch64"] ; For M1/M2
                 ]
  :repl-options {:init-ns solitaire-clojure.core}
  :main solitaire-clojure.core
  :profiles {:uberjar {:aot [solitaire-clojure.core]}})
