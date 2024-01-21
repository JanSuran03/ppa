(ns semestral.conversion
  (:require [semestral.parser :as parser]
            [semestral.types])
  (:import (clojure.lang IPersistentList IPersistentVector Keyword Symbol)
           (java.io Writer)
           (semestral.types Application Lambda)))

(defmulti clojurize class)

(defmethod clojurize :default
  [x]
  (throw (RuntimeException. (str "Cannot clojurize [" x "] of class " (class x)))))

(defmethod clojurize Lambda
  [{:keys [arg body]}]
  [(clojurize arg) (clojurize body)])

(defmethod clojurize Application
  [{:keys [f arg]}]
  (list (clojurize f) (clojurize arg)))

(defmethod clojurize String
  [s]
  s)

(defmethod clojurize Symbol
  [s]
  (name s))

(defmethod clojurize Character
  [c]
  (str c))

(defmethod clojurize Keyword
  [kw]
  (name kw))

(defmethod clojurize IPersistentVector
  [v]
  (mapv clojurize v))

(defmethod clojurize IPersistentList
  [l]
  (apply list (map clojurize l)))

(defmulti lambdulize class)

(defmethod lambdulize :default
  [x]
  (throw (RuntimeException. (str "Cannot lambdulize " x " of class " (class x)))))

(defmethod lambdulize String
  [s]
  s)

(defmethod lambdulize IPersistentVector
  [[arg body]]
  (Lambda. arg (lambdulize body)))

(defmethod lambdulize IPersistentList
  [[f x]]
  (Application. (lambdulize f) (lambdulize x)))

(defmethod lambdulize Number
  [n]
  (parser/church-number n))


(defprotocol StringConvertible
  (as-str [this]))

(def lambda-sym (atom "λ"))

(defn fix-lambda [] (reset! lambda-sym "L"))

(extend-protocol StringConvertible
  String
  (as-str [this] this)
  Character
  (as-str [this] (str this))
  Keyword
  (as-str [this] (name this))
  Lambda
  (as-str [{:keys [arg body]}]
    (str "(" @lambda-sym " " (as-str arg) " . " (as-str body) ")"))
  Application
  (as-str [{:keys [f arg]}]
    (str "(" (as-str f) " " (as-str arg) ")")))

(defmethod print-method Lambda
  [lambda ^Writer w]
  (.write w ^String (as-str lambda)))

(defmethod print-method Application
  [application ^Writer w]
  (.write w ^String (as-str application)))
