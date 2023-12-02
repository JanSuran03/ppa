(ns semestral.core
  (:require [semestral.conversion :as conv]
            [semestral.string-conversion :as to-str]
            [semestral.parser :as p]
            [semestral.predefs :refer :all]
            [semestral.types]))

(def expr-two (p/call INC INC ZERO))
(def expr-two-plus-five (p/call ADD 2 5))
(def expr-three-times-four (p/call MUL 3 4))