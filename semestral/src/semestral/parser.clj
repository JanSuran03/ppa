(ns semestral.parser
  (:require [clojure.string :as str]
            [semestral.types])
  (:import (semestral.types Application Lambda)))

(defn- parse-symbols [s]
  (str/split s #" "))

(declare church-number)

(defn- parse-1 [x]
  (cond (nil? x) nil
        (number? x) [(church-number x)]
        (string? x) (parse-symbols x)
        :else [x]))

(defn lambda [args body]
  ((fn expand [[arg & more]]
     (if more
       (Lambda. arg (expand more))
       arg))
   (conj (parse-symbols args) body)))

(def l lambda)

(defn application [& args]
  ((fn expand [[x y & more]]
     (let [[x y & more2] (concat (parse-1 x) (parse-1 y))
           more (concat more2 more)]
       (cond (seq more) (Application. (Application. x y) (expand more)) ; ((f x) y
             y (Application. x y)                           ; (f x)
             :else x)))                                     ; x
   args))

(def call application)

(defn church-number [n]
  (if (and (int? n) (>= n 0))
    (->> (reduce (fn [sz _]
                   (call "s" sz))
                 "z"
                 (range n))
         (lambda "s z"))
    (throw (RuntimeException. (str "Can only lambdulize non-negative integer: " n)))))
