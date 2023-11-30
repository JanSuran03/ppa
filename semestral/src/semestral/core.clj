(ns semestral.core
  (:import (java.io Writer)))

(defprotocol StringConvertible
  (as-str [this]))

(defrecord Lambda [arg body])

(defn lambda [& args]
  ((fn expand [[arg & more]]
     (if more
       (Lambda. arg (expand more))
       arg))
   args))

(def l lambda)

(defrecord Application [f arg])

(defn application [& args]
  ((fn expand [[arg & more]]
     (if more
       (Application. arg (expand more))
       arg))
   args))

(def call application)

(extend-protocol StringConvertible
  String
  (as-str [this] this)
  Character
  (as-str [this] (str this))
  Lambda
  (as-str [{:keys [arg body]}]
    (str "(λ " (as-str arg) " . " (as-str body) ")"))
  Application
  (as-str [{:keys [f arg]}]
    (str "(" (as-str f) " " (as-str arg) ")")))

(defmethod print-method Lambda
  [lambda ^Writer w]
  (.write w ^String (as-str lambda)))

(def T (l "t" "f" "t"))
(def F (l "t" "f" "f" ))
(def Z (l "s" "s" (call "s" "z")))
(def Y (l "f" (call (l "x" (call "f" (call "x" "x")))
                    (l "x" (call "f" (call "x" "x"))))))