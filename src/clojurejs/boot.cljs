;;; -*- Mode: Clojure -*-
;;; vi: set ft=clojure :

(defmacro apply [fun & args] `(.apply ~fun ~fun ~@args))
(defmacro true? [expr] `(== true ~expr))
(defmacro false? [expr] `(== false ~expr))
(defmacro undefined? [expr] `(== undefined ~expr))
(defmacro nil? [expr] `(== nil ~expr))
(defmacro count [x]
 `(inline ~(str (clojurejs.js/emit-str x) ".length")))
(defmacro empty? [s] `(or (nil? ~s) (= 0 (count ~s) 0)))
(defmacro not-empty? [s] `(and ~s (> (count ~s) 0)))
(defmacro not [expr] `(! ~expr))
(defmacro not= [expr1 expr2] `(!= ~expr1 ~expr2))
(defmacro when [pred & body] `(if ~pred (do ~@body)))
(defmacro when-not [pred & body] `(if (false? ~pred) (do ~@body)))
(defmacro unless [pred & body] `(if (false? ~pred) (do ~@body)))
(defmacro cond [& [pred consequent & alternates]]
  (if (coll? alternates)
    (if (= (first alternates) :else)
      `(if ~pred ~consequent ~(second alternates))
      `(if ~pred ~consequent (cond ~@alternates)))
    `(if ~pred ~consequent)))
(defmacro first [x] `(get ~x 0))
(defmacro second [x] `(get ~x 1))
(defmacro third [x] `(get ~x 2))
(defmacro last [x] `(get ~x (- (count ~x) 1)))
(defmacro array? [a] (let [c 'Array] `(instanceof ~a ~c)))
(defmacro string? [s] `(== "string" (typeof ~s)))
(defmacro number? [n] `(== "number" (typeof ~n)))
(defmacro boolean? [b] `(== "boolean" (typeof ~b)))
(defmacro join [sep seq] `(.join ~seq ~sep))
(defmacro str [& args] `(+ "" ~@args))
(defmacro inc! [arg] `(set! ~arg (+ 1 ~arg)))
(defmacro lvar [& bindings]
  `(inline
    ~(str "var "
          (clojure.string/join ","
            (map (fn [[vname vval]]
                   (str vname " = " (clojurejs.js/emit-str vval)))
                 (partition 2 bindings))))))
(defmacro doseq [[var seq] & body]
  `(do
     (lvar seq# ~seq ~var nil)
     (loop [i# 0]
       (when (< i# (count seq#))
         (set! ~var (get seq# i#))
         ~@body
         (recur (+ i# 1))))))

(def *gensym* 999)
(defn gensym []
  (inc! *gensym*)
  (str "_gensym" *gensym*))

(defn subvec [a s e]
  (let [e (or e (count a))
        r (new Array)]
    (loop [i (or s 0)]
      (if (< i e)
        (do
          (.push r (get a i))
          (recur (+ i 1)))
        r))))

(defn map? [m]
  (let [t (typeof m)]
    (not (or (== "string" t) (== "number" t) (== "boolean" t) (array? m)))))

(defn map [fun arr]
  (loop [r (new Array)
         i 0]
    (if (< i (count arr))
      (do
        (.push r (fun (get arr i)))
        (recur r (+ i 1)))
      r)))

(defn html-set-attrs [el attrs]
  (dokeys [k attrs] (.setAttribute el k (get attrs k))))

(defn html [spec]
  (let [html1 (fn [spec]
                (let [el (.createElement document (first spec))
                      kindex 1]
                  (when (map? (second spec))
                    (html-set-attrs el (second spec))
                    (set! kindex (+ 1 kindex)))
                  (loop [i kindex]
                    (when (< i (count spec))
                      (.appendChild el (html (get spec i)))
                      (recur (+ i 1))))
                  el))]
    (cond
     (undefined? spec) (.createTextNode document "")
     (string? spec) (.createTextNode document spec)
     (array? spec) (html1 spec)
     :else spec)))
