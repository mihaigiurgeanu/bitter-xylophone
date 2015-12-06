(defproject bitter-xylophone-js "0.1.0-SNAPSHOT"
  :plugins [[lein-cljsbuild "1.1.1"]]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [secretary "1.2.3"]
                 [shodan "0.4.2"]]
  :cljsbuild {:builds [{
                        ; The path to the top-level ClojureScript source directory:
                        :source-paths ["cljs"]
                        ; The standard ClojureScript compiler options:
                        ; (See the ClojureScript compiler documentation for details.)
                        :compiler {:output-to "resources/js/main.js"  ; default: target/cljsbuild-main.js
                                   :optimizations :whitespace
                                   :pretty-print true}}]})