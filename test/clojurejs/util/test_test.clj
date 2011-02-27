(ns clojurejs.util.test-test
  "Unit tests for utility functions used in tests"
  (:use [clojure.test :only [deftest is]]
        clojurejs.util.test
        clojure.contrib.mock))

(deftest eval-test
  (is (= 1 (js-eval 1)))
  (is (= "BOO-YAH" (js-eval "BOO-YAH")))
  (is (= [1 "two"] (js-eval [1 "two"])))
  (is (= {1 "two"} (js-eval ((fn [] {1 "two"}))))))

(defn- foo [] "imported")

(deftest import-test
  (js-import [foo]
    (is (= "imported" (js-eval (foo))))))

(defn- scope-test-helper []
  (js-eval
    (set! x (- x 1))
    (if (> x 0)
      (helper))))

(deftest scope-test
  (js-import [[helper scope-test-helper] foo]
    (expect [scope-test-helper (->> (times 2)
                                    (calls scope-test-helper))]
      (is (= 2 (js-eval
                 (def x 2)
                 (helper)
                 x))))))
