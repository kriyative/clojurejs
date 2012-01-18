;;; -*- Mode: Clojure -*-
;;; vi: set ft=clojure :

(defmacro apply [fun & args] `(.apply ~fun ~fun ~@args))
(defn true? [expr] (=== true expr))
(defn false? [expr] (=== false expr))
(defn undefined? [expr] (=== undefined expr))
(defn nil? [expr] (=== nil expr))
(defmacro count [x] `(inline ~(str (clojurejs.js/emit-str x) ".length")))
(defmacro not [expr] `(! ~expr))
(defn empty? [s] (or (undefined? s) (nil? s) (=== 0 (count s))))
(defn not-empty? [s] (not (empty? s)))
(defmacro contains? [m k]
  `(inline ~(str (clojurejs.js/emit-str k) " in " (clojurejs.js/emit-str m))))
(defmacro not= [expr1 expr2] `(!= ~expr1 ~expr2))
(defmacro when [pred & body] `(if ~pred (do ~@body)))
(defmacro when-not [pred & body] `(if (not ~pred) (do ~@body)))
(defmacro unless [pred & body] `(if (not ~pred) (do ~@body)))
(defmacro cond [& [pred consequent & alternates]]
  (if (coll? alternates)
    (if (= (first alternates) :else)
      `(if ~pred ~consequent ~(second alternates))
      `(if ~pred ~consequent (cond ~@alternates)))
    `(if ~pred ~consequent)))
(defn first [x] (get x 0))
(defn second [x] (get x 1))
(defn third [x] (get x 2))
(defn last [x] (get x (- (count x) 1)))
(defmacro isa? [a t]
  `(inline ~(str "(" (clojurejs.js/emit-str a) " instanceof " t ")")))
(defn array? [a] (isa? a "Array"))
(defn string? [s] (=== "string" (typeof s)))
(defn number? [n] (=== "number" (typeof n)))
(defn boolean? [b] (=== "boolean" (typeof b)))
(defn fn? [f] (== "function" (typeof f)))
(defmacro join [sep seq] `(.join ~seq ~sep))
(defn str [& args] (.join args ""))
(defn inc [arg] (+ 1 arg))
(defn dec [arg] (- arg 1))
(defmacro inc! [arg] `(set! ~arg (+ 1 ~arg)))
(defmacro dec! [arg] `(set! ~arg (- ~arg 1)))

(defmacro delete [arg] `(inline ~(str "delete " (clojurejs.js/emit-str arg))))

(defmacro lvar [& bindings]
  `(inline
    ~(str "var "
          (clojure.string/join ","
            (map (fn [[vname vval]]
                   (str vname " = " (clojurejs.js/emit-str vval)))
                 (partition 2 bindings))))))

(defmacro doseq [[var seq] & body]
  (let [seqsym (gensym)]
    `(do
       (lvar ~seqsym ~seq)
       (loop [i# 0]
         (when (< i# (count ~seqsym))
           (let [~var (get ~seqsym i#)]
             ~@body)
           (recur (+ i# 1)))))))

(defmacro dotimes [[var n] & body]
  (let [nsym (gensym)]
    `(do
       (lvar ~nsym ~n)
       (loop [~var 0]
         (when (< ~var ~nsym)
           ~@body
           (recur (+ ~var 1)))))))

(defn reduce [f val coll]
  (loop [i 0
         r val]
    (if (< i (count coll))
      (recur (+ i 1) (f r (get coll i)))
      r)))

(def *gensym* 999)
(defn gensym []
  (inc! *gensym*)
  (str "G__" *gensym*))

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
  (and m
       (not (or (contains? #{:string :number :boolean} (typeof m))
                (array? m)))))

(defn map [fun arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (.push r (fun (get arr i)))
        (recur r (+ i 1)))
      r)))

(defn remove [pred seq]
  (loop [r []
         i 0]
    (if (< i (count seq))
      (do
        (when-not (pred (get seq i))
          (.push r (get seq i)))
        (recur r (+ 1 i)))
      r)))

(defn filter [pred arr]
  (loop [r []
         i 0]
    (if (< i (count arr))
      (do
        (if (pred (get arr i)) (.push r (get arr i)))
        (recur r (+ i 1)))
      r)))

(defn merge
  "Merge the contents of map `m2' into map `m1' and return a new map."
  [m1 m2]
  (or (and m2
           (let [m {}]
             (dokeys [k m1] (if (.hasOwnProperty m1 k) (set! (get m k) (get m1 k))))
             (dokeys [k m2] (if (.hasOwnProperty m2 k) (set! (get m k) (get m2 k))))
             m))
      m1))

(defn select-keys [m ks]
  (let [m1 {}]
    (doseq [k ks]
      (if (.hasOwnProperty m k)
        (set! (get m1 k) (get m k))))
    m1))

(defn keys [m]
  (let [v []]
    (dokeys [k m]
      (if (.hasOwnProperty m k)
        (.push v k)))
    v))

(defn vals [m]
  (let [v []]
    (dokeys [k m]
      (if (.hasOwnProperty m k)
        (.push v (get m k))))
    v))

(defn html-set-attrs [el attrs]
  (dokeys [k attrs] (.setAttribute el k (get attrs k))))

(defn html [spec]
  (cond
   (undefined? spec) (.createTextNode document "")
   (string? spec) (.createTextNode document spec)
   (array? spec) (let [el (.createElement document (first spec))
                       kindex 1]
                   (when (map? (second spec))
                     (html-set-attrs el (second spec))
                     (set! kindex (+ 1 kindex)))
                   (loop [i kindex]
                     (when (< i (count spec))
                       (.appendChild el (html (get spec i)))
                       (recur (+ i 1))))
                   el)
   :else spec))

(defn html-str
  "Generate a string representation of the specified HTML spec."
  [spec]
  (let [map-str (fn [m]
                  (let [s []]
                    (dokeys [k m] (.push s (+ k "='" (get m k) "'")))
                    (join " " s)))]
    (if (array? spec)
      (join ""
            [(+ "<" (first spec)
                (if (map? (second spec)) (+ " " (map-str (second spec))) "")
                ">")
             (let [s []
                   kindex (if (map? (second spec)) 2 1)]
               (loop [i kindex]
                 (when (< i (count spec))
                   (.push s (html-str (get spec i)))
                   (recur (+ i 1))))
               (join "" s))
             (+ "</" (first spec) ">")])
      spec)))
