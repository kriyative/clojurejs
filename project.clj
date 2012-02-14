(defproject org.clojars.kriyative/clojurejs "1.2.18"
  :description "A naive Clojure to Javascript translator"
  :url "http://github.com/kriyative/clojurejs"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [lein-clojars "0.5.0"]
                     [lein-difftest "1.3.1"]
                     [rhino/js "1.7R2"]]
  :hooks [leiningen.hooks.difftest]
  :test-path "test")
