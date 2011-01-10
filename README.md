clojurejs
=========

clojurejs is a naive implementation of a Clojure subset language to Javascript.

License
=======

Eclipse Public License - v 1.0.

Keywords
========

The following keywords are implemented by the clojurejs translator:

  def, defn, defmacro, do, dokeys, fn, get, if, inline, length, let, loop, new, nil, recur, return, set!, try/catch/finally

Examples
========

Please note that the output from the following examples are pretty printed Javascript, which is not the default.

    (use 'clojurejs.js)

    ;; functions
    (js
     (defn test-fn [a]
       (let [b (+ a 1)
             c (+ b 1)]
         (+ a b c))))
  
    "test_fn = function(a) {
         return (function () {
             var
                 b = (a + 1),
                 c = (b + 1);
             return (a + b + c);
         })();
     }"
  
    ;; macros
    (js
     (defmacro nil? [x] `(== nil ~x))
     (if (nil? a) (print "is null")))
  
    " if ((null == a)) { print(\"is null\"); };"
  
    ;; special forms loop/recur
    (js
     (defn join [arr delim]
       (loop [str (get arr 0)
              i 1]
         (if (< i (length arr))
           (recur (+ str delim (get arr i))
                  (+ i 1))
           str))))
  
    "join = function(arr, delim) {
        return (function () {
            for (var str = arr[0],i = 1; true;) {
                if ((i < arr.length)) {
                    str = (str + delim + arr[i]);
                    i = (i + 1);
                    continue;
                } else {
                    return str;
                } break;
            }
        })();
     }"
