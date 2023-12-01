(ns semestral.core
  (:require [semestral.conversion :as conv]
            [semestral.string-conversion :as to-str]
            [semestral.parser :as p]
            [semestral.predefs :refer :all]
            [semestral.types]))

(def expr-two (p/call INC INC ZERO))
