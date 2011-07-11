;;; tests_boot.clj -- unit tests for clojurejs standard library

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2011. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojurejs.test-boot
  (:use [clojure.test :only [deftest is]]
        clojurejs.js
        clojurejs.util.test-rhino
        clojure.contrib.mock))

(def *boot-js* (tojs "src/clojurejs/boot.cljs"))

(deftest variables
  (is (= (js
           (lvar x 0)
           (set! x (+ x 1)))
         " var x = 0; x = (x + 1);")))

(deftest datastructures
  (is (= (js (contains? {:a 1} :a))
         "'a' in {'a' : 1}")))

(deftest types
  (is (= (js (array? [1 2 3]))
         "([1,2,3] instanceof Array)"))

  (is (= (js (isa? "foobar" "String"))
         "(\"foobar\" instanceof String)")))

(deftest doseq-test
  (is (= 120
         (js-eval
          (defn test []
            (let [prod 1]
              (doseq [i [1 2 3 4 5]]
                (set! prod (* prod i)))
              prod))
          (test))))

  (is (= [120 120]
           (js-eval
            (defn test []
              (let [p1 1
                    p2 1]
                (doseq [[x y] [[1 1] [2 2] [3 3] [4 4] [5 5]]]
                  (set! p1 (* p1 x)
                        p2 (* p2 y)))
                [p1 p2]))
            (test)))))

(deftest core-fns
  (is (= (js (defn has-foo? [] (contains? {:foo 1 :bar 2} :foo)))
         "has_foop = function () { return 'foo' in {'foo' : 1,'bar' : 2}; }"))

  (is (= true (js-eval* {:preload *boot-js*} (contains? {:foo 1 :bar 2} :foo))))

  (is (= {:foo 1 :baz 3} (js-eval* {:preload *boot-js*} (select-keys {:foo 1 :bar 2 :baz 3} [:foo :baz]))))

  (is (= [1 2 3] (js-eval* {:preload *boot-js*} (vals {:foo 1 :bar 2 :baz 3}))))

  (is (= ["foo" "bar" "baz"] (js-eval* {:preload *boot-js*} (keys {:foo 1 :bar 2 :baz 3}))))

  (is (= [2 4 6] (js-eval* {:preload *boot-js*} (filter (fn [x] (=== (% x 2) 0)) [1 2 3 4 5 6])))))
