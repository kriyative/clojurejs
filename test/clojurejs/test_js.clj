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
  (:use [clojure.test :only [deftest is]]
        clojurejs.js
        clojurejs.util.test-rhino
        clojure.contrib.mock))

(tojs "src/clojurejs/boot.cljs")

(deftest literals
  (is (= (js *print-pretty*) "__print_pretty__"))
  (is (= (js number?) "numberp"))
  (is (= (js foo-bar-baz) "foo_bar_baz"))
  (is (= (js inc!) "incf"))
  (is (= (js {:foo 1 :bar 2 :baz 3}) "{'foo' : 1,'bar' : 2,'baz' : 3}"))
  (is (= (js [:foo :bar :baz]) "['foo','bar','baz']"))
  (is (= (js #"^([a-z]*)([0-9]*)") "/^([a-z]*)([0-9]*)/"))
  (is (= (js \newline) "'\n'"))
  (is (= (js \a) "'a'")))

(deftest functions
  (is (= (js (+ 1 2 3)) "(1 + 2 + 3)"))
  (is (= (js (+ "foo" "bar" "baz")) "(\"foo\" + \"bar\" + \"baz\")"))
  (is (= (js (:test {:test 1 :foo 2 :bar 3}))
         "{'test' : 1,'foo' : 2,'bar' : 3}['test']"))
  (is (= (js (let [m {:test 1 :foo 2 :bar 3}] (:baz m 4)))
         "var m = {'test' : 1,'foo' : 2,'bar' : 3}; ('baz' in m ? m['baz'] : 4);"))
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

(deftest destructuring
  (is (= (js
          (defn test []
            (let [a 1
                  b (+ a 1)
                  c (+ b 1)]
              (+ a b c))))
         "test = function () { var a = 1, b = (a + 1), c = (b + 1); return (a + b + c);; }"))

  ;; & rest
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
         "test = function () { var _temp_1000 = Array.prototype.slice.call(arguments), a = _temp_1000[0], b = _temp_1000[1], r = _temp_1000.slice(2); return [(a + b),r]; }"))

  ;; :as
  (is (= (js
          (fn [a [b] [c d & e :as f] :as g] nil))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), a = _temp_1000[0], _temp_1001 = _temp_1000[1], b = _temp_1001[0], _temp_1002 = _temp_1000[2], c = _temp_1002[0], d = _temp_1002[1], e = _temp_1002.slice(2), f = _temp_1002, g = _temp_1000; return null; }")) 

  ;; map destructuring
  (is (= (js
          (fn [x {y :y, fred :fred}] fred))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), x = _temp_1000[0], _temp_1001 = _temp_1000[1], y = _temp_1001['y'], fred = _temp_1001['fred']; return fred; }"))

  (is (= (js
          (fn [[{x :x, {z :z} :y}]] z))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), _temp_1001 = _temp_1000[0], _temp_1002 = _temp_1001[0], x = _temp_1002['x'], _temp_1003 = _temp_1002['y'], z = _temp_1003['z']; return z; }"))

  ;; numbers as keys (this actually works)
  (is (= (js
          (fn [{a 1, b 2, :or {a 3}}]))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), _temp_1001 = _temp_1000[0], a = (1 in _temp_1001 ? _temp_1001[1] : 3), b = _temp_1001[2]; return null; }"))

  ;; :keys, :strs
  (is (= (js
          (fn [x {y :y, z :z :keys [a b]}] z))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), x = _temp_1000[0], _temp_1001 = _temp_1000[1], a = _temp_1001['a'], b = _temp_1001['b'], y = _temp_1001['y'], z = _temp_1001['z']; return z; }"))

  (is (= (js
          (fn [x {y :y, z :z :strs [a b]}] z))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), x = _temp_1000[0], _temp_1001 = _temp_1000[1], a = _temp_1001['a'], b = _temp_1001['b'], y = _temp_1001['y'], z = _temp_1001['z']; return z; }"))
                                        ; defaults
  (is (= (js
          (fn [x {y :y, z :z :or {y 1, z "foo"}}] z))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), x = _temp_1000[0], _temp_1001 = _temp_1000[1], y = ('y' in _temp_1001 ? _temp_1001['y'] : 1), z = ('z' in _temp_1001 ? _temp_1001['z'] : \"foo\"); return z; }"))

  (is (= (js
          (fn [x {y :y, z :z :keys [a b] :or {a 1, y :bleh}}] z))
         "function () { var _temp_1000 = Array.prototype.slice.call(arguments), x = _temp_1000[0], _temp_1001 = _temp_1000[1], a = ('a' in _temp_1001 ? _temp_1001['a'] : 1), b = _temp_1001['b'], y = ('y' in _temp_1001 ? _temp_1001['y'] : 'bleh'), z = _temp_1001['z']; return z; }"))

  ;; unsupported for now
  (is (thrown-with-msg? Exception #"& must be followed by"
        (js
         (fn [x y & {z :z}] z))))
  )

(deftest macros
  (is (= (js
          (if (nil? a) (print "is null")))
         "if ((null === a)) { print(\"is null\"); }")))
    
(deftest loops
  (is (= (js
          (defn join [arr delim]
            (loop [str (get arr 0)
                   i 1]
              (if (< i (get arr .length))
                (recur (+ str delim (get arr i))
                       (+ i 1))
                str))))
         "join = function (arr, delim) { for (var str = arr[0], i = 1; true;) { if ((i < arr.length)) { var _temp_1000 = [(str + delim + arr[i]),(i + 1)];\n str = _temp_1000[0]; i = _temp_1000[1]; continue; } else { return str; }; break; }; }")))

(deftest inline-if
  (is (= (js
          (defn test [a]
            ((if (> a 0) minus plus) a 1)))
         "test = function (a) { return (((a > 0) ? minus : plus))(a,1); }"))
  
  ;; implicit `null` alternate
  (is (= (js (defn test [a] (console.log (if (> a 0) a))))
         "test = function (a) { return console.log(((a > 0) ? a : null)); }")))

(deftest inline-primitives
  (is (= (js (defn isa? [i c] (inline "i instanceof c")))
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
         "test = function () { try { return (5 / 0); } catch (ex) { return console.log(ex); } finally { return 0; }; }"))

  (is (= (js (defn test [a] (if (< a 0) (throw (new Error "Negative numbers not accepted")))))
         "test = function (a) { if ((a < 0)) { throw new Error(\"Negative numbers not accepted\"); }; }")))

(deftest combo
  (is (= (js
          (defn test [a] (if (! (or (boolean? a) (string? a))) (first a))))
         "test = function (a) { if (!((\"boolean\" === typeof(a)) || (\"string\" === typeof(a)))) { return a[0]; }; }"))

  (is (= (js
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no"
             :else "don't know")))
         "test = function (a) { if (symbolp(a)) { return \"yes\"; } else { if ((\"number\" === typeof(a))) { return \"no\"; } else { return \"don't know\"; }; }; }"))

  (is (= (js
          (defn test [a]
            (cond
             (symbol? a) "yes"
             (number? a) "no")))
         "test = function (a) { if (symbolp(a)) { return \"yes\"; } else { if ((\"number\" === typeof(a))) { return \"no\"; }; }; }")))

(declare foo)

(deftest do-expression-test
  (js-import [foo]
    (expect [foo (->> (times once) (returns 0))]
      (is (= 123 (js-eval (do (def x (do (foo) 123)) x)))))
    (expect [foo (->> (times once) (returns 0))]
      (is (= 123 (js-eval (do (def x -1) (set! x (do (foo) 123)) x)))))))

(deftest if-expression-test
  (js-import [foo]
    (expect [foo (times 2)]
      (is (= 1 (js-eval
                 (do (if (do (foo) true)
                       (do (foo) 1)
                       (do (foo) 2)))))))
    (expect [foo (times 2)]
      (is (= 1 (js-eval
                 (if (do (foo) true)
                   (do (foo) 1)
                   (do (foo) 2))))))))

(deftest loop-expression-test
  (js-import [foo]
    (expect [foo (times 2)]
      (is (= -1 (js-eval
                 (def x (loop [i 1]
                          (foo)
                          (if (>= i 0) (recur (- i 2)) i)))
                 x))))
    (expect [foo (times 6)]
      (is (= -1 (js-eval
                 (loop [i (do (foo) 9)]
                   (if (> i 0)
                     (recur (do (foo) (- i 2)))
                     i))))))
    (expect [foo (times 6)]
      (is (= -1 (js-eval
                 ((fn [] ; create and call anonymous fn
                    (loop [i (do (foo) 9)] 
                      (if (> i 0)
                        (recur (do (foo) (- i 2)))
                        i))))))))))

(deftest let-expression-test
  (js-import [foo]
    (expect [foo (times 2)]
      (is (= 123 (js-eval (def x (let [y 123] (foo) y)) x)))
      (is (= 123 (js-eval (do (def x (let [y 123] (foo) y)) x)))))))

(deftest new-form-test
  (js-import [foo]
    (expect [foo (times 5)]
      (is (= 123 (js-eval (new (do (foo) Number) (do (foo) 123)))))
      (is (= 123 (js-eval (do (foo) (new (do (foo) Number) (do (foo) 123))))))
      
      (is (= (js (Number. 10)) "new Number(10)")))))
