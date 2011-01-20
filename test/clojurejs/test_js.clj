;;; tests_js.clj -- unit tests for clojurejs language core

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2011. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojurejs.test-js
  (:use [clojure.test :only [deftest is]])
  (:use clojurejs.js))

(deftest literals
  (is (= (js *print-pretty*) "__print_pretty__"))
  (is (= (js number?) "numberp"))
  (is (= (js foo-bar-baz) "foo_bar_baz"))
  (is (= (js inc!) "incf"))
  (is (= (js {:foo 1 :bar 2 :baz 3}) "{'foo' : 1,'bar' : 2,'baz' : 3}"))
  (is (= (js [:foo :bar :baz]) "['foo','bar','baz']"))
  (is (= (js #"^([a-z]*)([0-9]*)") "/^([a-z]*)([0-9]*)/")))

(deftest functions
  (is (= (js (+ 1 2 3)) "(1 + 2 + 3)"))
  (is (= (js (+ "foo" "bar" "baz")) "(\"foo\" + \"bar\" + \"baz\")"))
  (is (= (js (:test {:test 1 :foo 2 :bar 3}))
         "{'test' : 1,'foo' : 2,'bar' : 3}['test']"))
  
  (is (= (js (append '(:foo bar baz) '(quux)))
         "append(['foo','bar','baz'], ['quux'])"))

  (is (= (js (fn [a b] (+ a b)))
         "function (a, b) { return (a + b); }"))
  
  (is (= (with-pretty-print (js (fn "Some func does stuff" [x] (+ x 1))))
         "function (x) {\n    /* Some func does stuff */\n    return (x + 1);\n}"))

  (is (= (with-pretty-print (js (fn "Some func\ndoes stuff" [x] (+ x 1))))
         "function (x) {\n    /* Some func\n       does stuff */\n    return (x + 1);\n}"))

  (is (= (js (defn foo [a b] (+ a b)))
         "foo = function (a, b) { return (a + b); }"))

  (is (= (js (defn foo [c] (.methodOf c)))
         "foo = function (c) { return c.methodOf(); }"))

  (is (= (js
          (defn test []
            (let [a 1] (log (* a a)))
            (let [a 2] (log (* a a)))))
         "test = function () { var a = 1; log((a * a));; var a = 2; return log((a * a));; }"))
  (is (= (js
          (defn test []
            (let [a 1] (log (* a a)))
            (do (log "test") (+ 1 1))))
         "test = function () { var a = 1; log((a * a));;  log(\"test\"); return (1 + 1);; }")))

(deftest property-access
  (is (= (js (get map :key))
         "map['key']"))
  (is (= (js (:key map))
         "map['key']"))
  (is (= (js (get map .key))
         "map.key")))

(deftest property-access-default
  (is (= (js (get map :key default))
         "('key' in map ? map['key'] : default)"))

  (is (= (js (get map .key default))
         "('key' in map ? map.key : default)")))

(deftest let-destructuring
  (is (= (js
          (defn test []
            (let [a 1
                  b (+ a 1)
                  c (+ b 1)]
              (+ a b c))))
         "test = function () { var a = 1, b = (a + 1), c = (b + 1); return (a + b + c);; }"))

  (is (= (js
          (defn test []
            (let [[a b & r] [1 2 3 4]]
              [(+ a b) r])))
         "test = function () { var _temp_1000 = [1,2,3,4], a = _temp_1000[0], b = _temp_1000[1], r = _temp_1000.slice(2); return [(a + b),r];; }"))

  (is (= (js
          (defn test [[a b & r]]
            [(+ a b) r]))
         "test = function () { var _temp_1000 = Array.prototype.slice.call(arguments), _temp_1001 = _temp_1000[0], a = _temp_1001[0], b = _temp_1001[1], r = _temp_1001.slice(2); return [(a + b),r]; }"))

  (is (= (js
          (defn test [a b & r]
            [(+ a b) r]))
         "test = function () { var _temp_1000 = Array.prototype.slice.call(arguments), a = _temp_1000[0], b = _temp_1000[1], r = _temp_1000.slice(2); return [(a + b),r]; }")))

(deftest macros
  (is (= (js
          (defmacro nil? [x] `(== nil ~x))
          (if (nil? a) (print "is null")))
         " if ((null == a)) { print(\"is null\"); };")))
    
(deftest loops
  (is (= (js
          (defn join [arr delim]
            (loop [str (get arr 0)
                   i 1]
              (if (< i (get arr .length))
                (recur (+ str delim (get arr i))
                       (+ i 1))
                str))))
         "join = function (arr, delim) { for (var str = arr[0], i = 1; true;) { if ((i < arr.length)) {  str = (str + delim + arr[i]); i = (i + 1); continue; } else { return str; }; break; }; }")))

(deftest inline-if
  (is (= (js
          (defn test [a]
            ((if (> a 0) minus plus) a 1)))
         "test = function (a) { return (((a > 0) ? minus : plus))(a,1); }")))

(deftest inline-primitives
  (is (= (js (defn isa? [i c] (inline "return i instanceof c")))
         "isap = function (i, c) { return i instanceof c; }")))

(deftest try-catch-finally
  (is (= (js
          (defn test []
            (try
              (/ 5 0)
              (catch ex
                  (console.log ex))
              (finally
               0))))
         "test = function () { try { return (5 / 0); } catch (ex) { return console.log(ex); } finally { return 0; }; }")))

(deftest combo
  (is (= (js
          (defmacro boolean? [b] `(== "boolean" (typeof ~b)))
          (defmacro string? [s] `(== "string" (typeof ~s)))
          (defmacro first [x] `(get ~x 0))
          (defn test [a] (if (! (or (boolean? a) (string? a))) (first a))))
         " test = function (a) { if (!((\"boolean\" == typeof(a)) || (\"string\" == typeof(a)))) { return a[0]; }; };"))

  (is (= (js
          (defmacro number? [n] `(== "number" (typeof ~n)))
          (defmacro cond [& [pred consequent & alternates]]
            (if (coll? alternates)
              (if (= (first alternates) :else)
                `(if ~pred ~consequent ~(second alternates))
                `(if ~pred ~consequent (cond ~@alternates)))
              `(if ~pred ~consequent)))
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no"
             :else "don't know")))
         " test = function (a) { if (symbolp(a)) { return \"yes\"; } else { if ((\"number\" == typeof(a))) { return \"no\"; } else { return \"don't know\"; }; }; };"))

  (is (= (js
          (defmacro number? [n] `(== "number" (typeof ~n)))
          (defmacro cond [& [pred consequent & alternates]]
            (if (coll? alternates)
              (if (= (first alternates) :else)
                `(if ~pred ~consequent ~(second alternates))
                `(if ~pred ~consequent (cond ~@alternates)))
              `(if ~pred ~consequent)))
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no")))
         " test = function (a) { if (symbolp(a)) { return \"yes\"; } else { if ((\"number\" == typeof(a))) { return \"no\"; }; }; };")))
