(ns semestral.predefs
  (:require [semestral.parser :refer :all]))

(def TRUE (l "t f" "t"))
(def FALSE (l "t f" "f"))
(def ZERO (l "s z" (call "s z")))
(def Y-COMB (l "f" (call (l "x" (call "f" (call "x x")))
                         (l "x" (call "f" (call "x x"))))))
(def ADD (l "x y s z" (call "x s" (call "y s z"))))
(def DEC (l "x s z" (call "x"
                          (l "f g" (call "g" (call "f s")))
                          (l "g" "z")
                          (l "u" "u"))))
(def SUB (l "m n" (call "n" DEC "m")))
(def AND (l "x y" (call "x y x")))
(def OR (l "x y" (call "x x y")))
(def NOT (l "x y" (call "x" FALSE TRUE)))
(def MUL (l "x y s" (call "x" (call "y s"))))
(def NAND (l "x y" (call (call "x y x")
                         FALSE
                         TRUE)))
(def INC (l "n s z" (call "s" (call "n s z"))))
(def ZERO? (l "n" (call "n" FALSE TRUE)))
(def FACT (l "f n" (call ZERO? "n" 1 (call "f" (call DEC "n")))))

(def predefs (->> (ns-publics *ns*)
                  (filter (fn [[sym _]] (every? #(or (not (Character/isAlphabetic (int %)))
                                                     (Character/isUpperCase ^Character %))
                                                (name sym))))
                  (map (fn [[sym var]]
                         [(str sym) (deref var)]))
                  (into {})))