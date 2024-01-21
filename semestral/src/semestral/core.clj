(ns semestral.core
  (:refer-clojure :exclude [->])
  (:require [semestral.conversion :as conv]
            [semestral.evaluator :as eval :refer [run it-last run-result run-steps]]
            [semestral.parser :as p]
            [semestral.predefs :refer :all :as predefs]
            [semestral.types]))

