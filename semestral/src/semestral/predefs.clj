(ns semestral.predefs
  (:require [semestral.parser :refer :all]))

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
(def INC (l "n s z" (call "s" (call "n s z"))))
(def ZERO? (l "n" (call "n" FALSE TRUE)))
(def FACT (l "x" ()))

(defmacro sym-map [& syms]
  (into {} (map #(vector (str %) %)) syms))

(def predefs (sym-map TRUE FALSE ZERO Y-COMB ADD AND OR NOT MUL NAND))
