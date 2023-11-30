(ns semestral.core
  (:require [clojure.string :as str])
  (:import (clojure.lang Keyword)
           (java.io Writer)))

(defprotocol StringConvertible
  (as-str [this]))

(defrecord Lambda [arg body])

(defn- parse-symbols [s]
  (str/split s #" "))

(defn lambda [args body]
  ((fn expand [[arg & more]]
     (if more
       (Lambda. arg (expand more))
       arg))
   (conj (parse-symbols args) body)))

(def l lambda)

(defrecord Application [f arg])

(defn application [& args]
  ((fn expand [[arg & more]]
     (cond (string? arg) (let [[sym1 sym2 & syms] (parse-symbols arg)]
                           (cond sym2 (let [first-two (Application. sym1 sym2) ; first two are symbols
                                            more (concat syms more)]
                                        (if (seq more)
                                          (Application. first-two (expand more))
                                          first-two))
                                 more (Application. sym1 (expand more)) ; first is symbol, second is call
                                 :else sym1))               ; only 1 symbol
           more (Application. arg (expand more))
           :else arg))
   args))

(def call application)

(extend-protocol StringConvertible
  String
  (as-str [this] this)
  Character
  (as-str [this] (str this))
  Keyword
  (as-str [this] (name this))
  Lambda
  (as-str [{:keys [arg body]}]
    (str "(λ " (as-str arg) " . " (as-str body) ")"))
  Application
  (as-str [{:keys [f arg]}]
    (str "(" (as-str f) " " (as-str arg) ")")))

(defmethod print-method Lambda
  [lambda ^Writer w]
  (.write w ^String (as-str lambda)))

(def TRUE (l "t f" "t"))
(def FALSE (l "t f" "f"))
(def ZERO (l "s z" (call "s z")))
(def Y-COMB (l "f" (call (l "x" (call "f" (call "x x")))
                         (l "x" (call "f" (call "x x"))))))
(def ADD (l "x y s z" (call "x s" (call "y s z"))))
(def AND (l "x y" (call "x y x")))
(def OR (l "x y" (call "x x y")))
(def NOT (l "x y" (call "x" FALSE TRUE)))
(def MUL (l "x y s" (call "x" (call "y s"))))
(def NAND (l "x y" (call (call "x y x")
                    FALSE
                    TRUE)))

(defmacro sym-map [& syms]
  (into {} (map #(vector (str %) %)) syms))

(def predefs (sym-map TRUE FALSE ZERO Y-COMB ADD AND OR NOT MUL NAND))
