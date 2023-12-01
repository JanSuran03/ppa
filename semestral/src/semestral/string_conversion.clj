(ns semestral.string-conversion
  (:require [semestral.types])
  (:import (clojure.lang Keyword)
           (java.io Writer)
           (semestral.types Application Lambda)))

(defprotocol StringConvertible
  (as-str [this]))

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

(defmethod print-method Application
  [application ^Writer w]
  (.write w ^String (as-str application)))
