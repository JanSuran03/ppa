(ns semestral.core
  (:refer-clojure :exclude [->])
  (:require [semestral.conversion :as conv]
            [semestral.evaluator :as eval]
            [semestral.parser :as p]
            [semestral.predefs :refer :all :as predefs]
            [semestral.types]))

(defn evaluate [expr]
  (eval/run-the-turing-fkin-machine expr))

(defmacro with-interval [interval & body]
  `(binding [eval/*interval* ~interval]
     ~@body))

(defmacro no-step-by-step [& body]
  `(binding [eval/*step-by-step-eval* false
             eval/*interval* 0]
     ~@body))
