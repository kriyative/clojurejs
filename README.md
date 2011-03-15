# clojurejs

clojurejs is a naive implementation of a Clojure subset language to Javascript translator. clojurejs is an attempt to implement the predictable semantics in the generated Javascript. Some of its features are:

* Consistent scoping in ``let`` and ``loop/recur`` forms
* Macros with ``defmacro``
* Implicit ``return`` from all forms
* ``loop/recur`` translates to Javascript ``for`` loops
* Translates Clojure vectors, strings, keywords, symbols and maps to Javascript equivalents

# License

Eclipse Public License - v 1.0.

# Features

## Scoping Rules

clojurejs implements ``let`` and ``loop/recur`` with anonymous functions in Javascript to ensure consistent scoping rules. See the Examples section for samples of ``let`` expansions.

## Implicit Return

clojurejs tries to be exhaustive about implementing _implicit return_ in the generated Javascript. This is one area where more unit tests are much needed.

## Reserved Symbols/Forms

The following reserved forms are implemented by the clojurejs translator:

    def, defn, defmacro, do, dokeys, fn, get, if, inline, length, let, loop, new, nil, recur, return, set!, try/catch/finally

## Symbol Translation

The translator rewrites Lisp style symbols like ``*foo*``, ``number?``, and ``inc!`` to acceptable Javascript forms such as the following:

    Lisp         Javascript
    ----         ----------
    foo-bar      foo_bar
    *foo*        _foo_
    number?      numberp
    inc!         incf
    and          &&
    or           ||

## Operators

clojurejs recognizes the following standard Javascript operators:

    ++ -- !
    && || + - * / %
    > >= < <= == === != !==
    instanceof

## Arrays and Objects

Javascript Array and Object member access is via the ``get`` form, e.g.,

    (def arr [:foo :bar :baz])
    (get arr 0)                ;; => :foo
    (set! (get arr 0) :quux)
    arr                        ;; => [:quux :bar :baz]

    ;; set the fillStyle of a canvas context
    (set! (get ctx 'fillStyle) "red")

## Special Forms

clojurejs introduces a couple of special forms, to support Javascript specific functionality.

### dokeys

_dokeys_ is a Clojure (subset) equivalent of the Javascript _for..in_ loop.

    (dokeys [k attrs] (.setAttribute el k (get attrs k)))

translates to the following Javascript:

    for (var k in attrs) { el.setAttribute(k, attrs[k]); }

### inline

_inline_ is an escape hatch to introduce inlined Javascript code, e.g,

    (defn isa? [i c] (inline "return i instanceof c"))

translates to the following Javascript:

    isap = function(i, c) { return i instanceof c; }

# Examples

Please note that the output from the following examples are pretty printed Javascript, which is not the default.

To get started:

    (use 'clojurejs.js)

## Functions

    (js
     (defn test-fn [a]
       (let [b (+ a 1)
             c (+ b 1)]
         (+ a b c))))

translates to the following Javascript:

    test_fn = function (a) {
        var 
            b = (a + 1), 
            c = (b + 1);
        return (a + b + c);;
    }

## Destructuring

Function args and let bindings can be destructured, e.g.:

    (js
     (defn test-fn [num-list]
       (let [[a b] num-list
             c (+ b 1)]
         (+ a b c))))

which translates to:

    test_fn = function (num_list) {
        var 
            _temp_1000 = num_list, a = _temp_1000[0], b = _temp_1000[1], 
            c = (b + 1);
        return (a + b + c);;
    }

Map destructuring (aka associative destructuring) allows to bind the properties of a JavaScript object to names, including ``:keys`` syntax and defaults for missing keys (using ``:or``). For example:

    (js
     (defn test-fn [square]
       (let [{:keys [x1 y1 x2 y2]} square
             area (* (- x2 x1) (- y2 y1))]
         (do-stuff area))))

becomes:

    test_fn = function (square) {
        var
	    _temp_1000 = square,
	    x1 = _temp_1000['x1'], x2 = _temp_1000['x2'],
	    y1 = _temp_1000['y1'], y2 = _temp_1000['y2'],
	    area = ((x2 - x1) * (y2 - y1));
    return do_stuff(area);;


When destructuring an array, aliasing the whole array with ``:as`` works the same as in Clojure. Destructuring bindings may be nested arbitrarily.

## Macros

    (js
     (defmacro nil? [x] `(== nil ~x))
     (if (nil? a) (print "is null")))

translates to the following Javascript:

    if ((null == a)) { print("is null"); };

## loop/recur

    (js
     (defn join [arr delim]
       (loop [str (get arr 0)
              i 1]
         (if (< i (length arr))
           (recur (+ str delim (get arr i))
                  (+ i 1))
           str))))

translates to the following Javascript:

    join = function (arr, delim) {
            for (var str = arr[0], i = 1; true;) {
                if ((i < arr.length)) {
                    str = (str + delim + arr[i]);
                    i = (i + 1);
                    continue;
                } else {
                    return str;
                };
                break;
            };
    }

## try/catch/finally

    (js
     (defn test []
       (try
         (/ 5 0)
         (catch ex
             (console.log ex))
         (finally
          0))))

translates to:

    test = function() {
        try {
            return (5 / 0);
        } catch (ex) {
            return console.log(ex);
        } finally {
            return 0;
        };
    }

# Caveats

The _defn_ form doesn't support multiple arity forms or keyword args. Doc-strings are supported and emitted as comments if pretty-printing is turned on.

Currently, there's no support for namespaces. Macro expanders are defined in a global ref, which is preserved between successive invocations of the translator.

There's lots of room for improvements in the generated Javascript, from performance tweaks to better formatting of expressions.

# boot.cljs

The file `boot.cljs' includes some useful macros and utility functions implemented in clojurejs.

## (html _spec_)

The _html_ function in boot.cljs implements a minimal HTML templating facility which is similar to hiccup, but is executed on the browser side.

    (jq
     (defn test []
       (.append ($ document.body)
                (html
                 [:div {:id "container"}
                  [:span {:class "title"} "Lorem ipsum blah blah"]
                  [:ul {:id "hmenu"}
                   [:li [:a {:class "link_login"} "Login"]]
                   [:li [:a {:class "link_signup"} "Signup"]]
                   [:li [:a {:class "link_about"} "About"]]
                   [:li [:a {:class "link_contact"} "Contact"]]]]))))

Invoking _(test)_ on the browser side would create and add the dom tree specified in the array structure to the document body.

## (html-str __spec__)

The __html-str__ function is similar to the __html__ function, except it returns a HTML string represenation of the specified __spec__. e.g.,

    (html-str
     [:ul
      [:li [:a {:href "#"} "link1"]]
      [:li [:a {:href "#"} "link2"]]])

at runtime, would generate:

    "<ul><li><a href='#'>link1</a></li><li><a href='#'>link2</a></li></ul>"

# Release notes

* 1.2.6

  - bugfixes for various javascript emitter issues

  - integrates with the [Rhino javascript engine](http://www.mozilla.org/rhino/) for running unit tests on the emitted javascript, thanks to [Anton Frolov](http://github.com/antonf).

* 1.2.0

  - adds support for destructuring in `fn`, `defn`, and `let` forms

  - a little optimization in `let` and `loop` forms, the outermost `function` block is elided

  - added a sample macro implementation of the `lvar` form, to add local var declarations in the generated Javascript
