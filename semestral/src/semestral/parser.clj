(ns semestral.parser
  (:require [clojure.string :as str]
            [semestral.types])
  (:import (semestral.types Application Lambda)))

(defn- parse-symbols [s]
  (str/split s #" "))

(declare church-number)

(defn lambda [args body]
  ((fn expand [[arg & more]]
     (if more
       (Lambda. arg (expand more))
       arg))
   (conj (parse-symbols args) body)))

(def l lambda)

(defn maybe-church-number [x]
  (if (number? x)
    (church-number x)
    x))

(defn application [x y]
  (Application. (maybe-church-number x) (maybe-church-number y)))

(def call application)

(defn church-number [n]
  (if (and (int? n) (>= n 0))
    (->> (reduce (fn [sz _]
                   (call "s" sz))
                 "z"
                 (range n))
         (lambda "s z"))
    (throw (RuntimeException. (str "Can only lambdulize non-negative integer: " n)))))
