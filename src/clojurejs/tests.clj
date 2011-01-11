;;; tests.clj -- unit tests for clj2js

;; Ram Krishnan, http://cynojure.posterous.com/

;; Copyright (c) Ram Krishnan, 2011. All rights reserved.  The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojurejs.tests
  (:use clojure.contrib.test-is)
  (:use clojurejs.js))

(deftest test-1
  (is (= (js *print-pretty*) "__print_pretty__"))
  (is (= (js number?) "numberp"))
  (is (= (js foo-bar-baz) "foo_bar_baz"))
  (is (= (js inc!) "incf"))
  (is (= (js {:foo 1 :bar 2 :baz 3}) "{'foo' : 1,'bar' : 2,'baz' : 3}"))
  (is (= (js [:foo :bar :baz]) "['foo','bar','baz']"))
  (is (= (js #"^([a-z]*)([0-9]*)") "/^([a-z]*)([0-9]*)/"))
  (is (= (js (+ 1 2 3)) "(1 + 2 + 3)"))
  (is (= (js (+ "foo" "bar" "baz")) "(\"foo\" + \"bar\" + \"baz\")"))
  
  (is (= (js (append '(:foo bar baz) '(quux)))
         "append(['foo','bar','baz'], ['quux'])"))

  (is (= (js (fn [a b] (+ a b)))
         "function (a, b) { return (a + b); }"))

  (is (= (with-pretty-print (js (fn "Some func\ndoes stuff" [x] (+ x 1))))
         "function (x) {\n    /* Some func\n       does stuff */\n    return (x + 1);\n}"))

  (is (= (js (defn foo [a b] (+ a b)))
         "foo = function(a, b) { return (a + b); }"))
  
  (is (= (js
          (defn test []
            (let [a 1
                  b (+ a 1)
                  c (+ b 1)]
              (+ a b c))))

         "test = function() { return (function () { var  a = 1, b = (a + 1), c = (b + 1); return (a + b + c);  })(); }"))
  
  (is (= (js
          (defmacro nil? [x] `(== nil ~x))
          (if (nil? a) (print "is null")))
         " if ((null == a)) { print(\"is null\"); };"))
    
  (is (= (js
          (defn join [arr delim]
            (loop [str (get arr 0)
                   i 1]
              (if (< i (length arr))
                (recur (+ str delim (get arr i))
                       (+ i 1))
                str))))
         "join = function(arr, delim) { return (function () { for (var str = arr[0],i = 1; true;) { if ((i < arr.length)) {  str = (str + delim + arr[i]); i = (i + 1); continue; } else { return str; }; break; } })(); }"))

  (is (= (js
          (defmacro boolean? [b] `(== "boolean" (typeof ~b)))
          (defmacro string? [s] `(== "string" (typeof ~s)))
          (defmacro first [x] `(get ~x 0))
          (defn test [a] (if (! (or (boolean? a) (string? a))) (first a))))
         " test = function(a) { if (!((\"boolean\" == typeof(a)) || (\"string\" == typeof(a)))) { return a[0]; }; };"))

  (is (= (js
          (defmacro number? [n] `(== "number" (typeof ~n)))
          (defmacro cond [& [pred consequent & alternates]]
            (if (and (coll? alternates) (= (first alternates) :else))
              `(if ~pred
                 ~consequent
                 ~(second alternates))
              `(if ~pred
                 ~consequent
                 (cond ~@alternates))))
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no"
             :else "don't know")))
         " test = function(a) { if (symbolp(a)) { return \"yes\"; } else { if ((\"number\" == typeof(a))) { return \"no\"; } else { return \"don't know\"; }; }; };"))

  (is (= (js
          (defn test [a]
            ((if (> a 0) minus plus) a 1)))
         "test = function(a) { return (((a > 0) ? minus : plus))(a,1); }"))

  (is (= (js (defn isa? [i c] (inline "return i instanceof c")))
         "isap = function(i, c) { return i instanceof c; }"))

  (is (= (js
          (defn test []
            (try
              (/ 5 0)
              (catch ex
                  (console.log ex))
              (finally
               0))))

         "test = function() { try { return (5 / 0); } catch (ex) { return console.log(ex); } finally { return 0; }; }"))

  )

;; (run-tests 'clojurejs.tests)
