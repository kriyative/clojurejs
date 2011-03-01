(ns clojurejs.util.test
  "Utility functions to help testing clojurejs."
  (:use [clojurejs.js :only [js]])
  (:import (org.mozilla.javascript Context ScriptableObject NativeArray
                                   NativeObject NativeJavaObject)))

(def ^{:private true} *scope* nil)
(def ^{:private true} *context* nil)

(declare wrap-value unwrap-value)

(defn call-in-new-js-context [body-fn]
  "Calls body-fn function in Rhino context prepared to execute tests."
  (try
    (let [ctx (Context/enter)
          scope (.initStandardObjects ctx)]
      (.setJavaPrimitiveWrap (.getWrapFactory ctx) false)
      (ScriptableObject/putProperty scope "_clj_wrap" wrap-value)
      (ScriptableObject/putProperty scope "_clj_unwrap" unwrap-value)
      (.evaluateString ctx scope
                       (str "function _clj_importfn(n,v){"
                            "this[n]=function(){"
                            "var args=[];"
                            "for(var i=0;i<arguments.length;++i){"
                            "args[i]=_clj_unwrap.invoke(arguments[i])"
                            "}"
                            "var f=v.deref(),"
                            "r=_clj_wrap.invoke(f.invoke.apply(f,args));"
                            "return r}}")
                       "<test-util>" 1 nil)
      (binding [*scope* scope
                *context* ctx]
        (body-fn)))
    (finally
     (when (Context/getCurrentContext)
       (Context/exit)))))

(defn call-in-new-js-scope [body-fn]
  "Any changes to JavaScript scope done by body-fn will be rolled back upon
  return from this function."
  (if (nil? *context*)
    (call-in-new-js-context #(call-in-new-js-scope body-fn))
    (let [clean-scope (.newObject *context* *scope*)]
      (.setPrototype clean-scope *scope*)
      (.setParentScope clean-scope nil)
      (binding [*scope* clean-scope]
        (body-fn)))))

(defn- js-object-to-clj-map [obj seen]
  (into {} (for [id (.getIds obj)]
             (let [key (if (string? id)
                         (keyword id)
                         (unwrap-value id))
                   raw-val (ScriptableObject/getProperty obj id)
                   val (unwrap-value raw-val seen)]
               [key val]))))

(defn- js-array-to-clj-vector [obj seen]
  (into [] (map #(unwrap-value
                  (ScriptableObject/getProperty obj %)
                  seen) (.getIds obj))))

(defn- unwrap-value
  ([obj] (unwrap-value obj nil))
  ([obj seen]
     (let [new-seen (conj seen obj)]
       (when-not (nil? (some #(identical? obj %) seen))
         (throw
          (RuntimeException. "Circular reference in unwrapped JS objects.")))
       (cond
        (instance? NativeJavaObject obj) (.unwrap obj)
        (instance? NativeObject obj) (js-object-to-clj-map obj new-seen)
        (instance? NativeArray obj) (js-array-to-clj-vector obj new-seen)
        :else obj))))

(defn- wrap-value [obj]
  (cond
   (sequential? obj) (.newArray *context* *scope* (to-array obj))
   (map? obj) (let [jsobj (.newObject *context* *scope*)]
                (doseq [[k v] obj]
                  (let [wrapped-key (if (keyword? k) (name k) (wrap-value k))]
                    (.put jsobj wrapped-key jsobj (wrap-value v))))
                jsobj)
   (or (and (instance? Long obj)
            (<= (Math/abs obj) 0x20000000000000))
       (instance? Integer obj)
       (instance? Short obj)
       (instance? Byte obj)
       (instance? Float obj)) (double obj)
   :else (Context/javaToJS obj *scope*)))

(defn- call-js-fn [name & args]
  (let [js-fn (ScriptableObject/getProperty *scope* name)]
    (.call js-fn *context* *scope* *scope* (object-array args))))

(defn js-import* [name fn-var]
  "Import clojure function to JavaScript root object under given name."
  (when (.isMacro fn-var)
    (throw (RuntimeException. "Can't import macros.")))
  (call-js-fn "_clj_importfn" name fn-var))

(defn simplify-import-decl [ns decl]
  (let [reslv (fn [s]
                (let [s (cond (string? s) (symbol s)
                              (symbol? s) s)]
                  (ns-resolve ns s)))]
    (cond
     (string? decl) [decl (reslv decl)]
     (symbol? decl) [(name decl) (reslv decl)]
     (sequential? decl) (let [rn (first decl)
                              name (cond (string? rn) rn
                                         (symbol? rn) (name rn))
                              var (reslv (second decl))]
                          [name var]))))

(defmacro js-import [imports & body]
  "imports => [ import-spec* ]
  import-spec => symbol | [new-name symbol-or-var]

  Import spec can be just symbol, then the name of imported function will be
  the same as name of symbol. Or it can be list or vector of two elements:
  first is name of imported function in JavaScript, second is symbol or var
  representing a function to import."
  (let [imports (map #(simplify-import-decl *ns* %) imports)]
    `(call-in-new-js-scope
      (fn []
        ~@(map (fn [x] `(js-import* ~@x)) imports)
        ~@body))))

(defn js-eval* [code]
  "Evaluate JavaScript code."
  (call-in-new-js-scope
   (fn []
     (try
       (unwrap-value
        (.evaluateString *context* *scope* code "<test-util>" 1 nil))
       (catch Exception e
         (println "JavaScript Code:")
         (println code)
         (throw (RuntimeException. "Exception evaluating JavaScript" e)))))))

(defmacro js-eval [& body]
  "Compiles expressions in body to JavaScript and evaluate the result."
  `(js-eval* (js ~@body)))
