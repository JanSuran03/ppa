(ns semestral.predefs
  (:refer-clojure :exclude [->])
  (:require [semestral.conversion :as conv]
            [semestral.evaluator :as eval]
            [semestral.parser :refer :all]))

(defn -> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (recur (call x (first forms)) (next forms))
      x)))

(def TRUE (l "t f" "t"))
(def FALSE (l "t f" "f"))
(def ZERO (l "s z" "z"))
(def Y-COMB (l "f" (-> (l "x" (-> "f" (-> "x" "x")))
                       (l "x" (-> "f" (-> "x" "x"))))))
(def ADD (l "x y s z" (-> "x" "s" (-> "y" "s" "z"))))
(def DEC (l "x s z" (-> "x"
                        (l "f g" (-> "g" (-> "f" "s")))
                        (l "g" "z")
                        (l "u" "u"))))
(def SUB (l "m n" (-> "n" DEC "m")))
(def AND (l "x y" (-> "x" "y" "x")))
(def OR (l "x y" (-> "x" "x" "y")))
(def NOT (l "x" (-> "x" FALSE TRUE)))
(def MUL (l "x y s" (-> "x" (-> "y" "s"))))
(def NAND (l "x y" (-> (-> "x" "y" "x")
                       FALSE
                       TRUE)))
(def INC (l "n s z" (-> "s" (-> "n" "s" "z"))))
(def ZERO? (l "n" (-> "n" (l "x" FALSE) TRUE)))
(def FACT (l "f n" (-> ZERO? "n" 1 (-> MUL "n" (-> "f" (-> DEC "n"))))))
(def XOR (l "x y" (-> "x" (-> NOT "y") "y")))
(def MOD2 (l "n" (-> "n" (l "x t f" (-> "x" "f" "t"))
                     TRUE
                     ZERO
                     (-> INC ZERO))))
(def RECURSION (l "f n m" (-> ZERO? "n"
                              (-> ZERO? "m"
                                  1
                                  (-> ADD "m" (-> "f" (-> DEC "m") 0)))
                              (-> ZERO? "m"
                                  (-> ADD "n" (-> "f" ZERO (-> DEC "n")))
                                  (-> ADD (-> "f" "n" (-> DEC "m")) (-> "f" (-> DEC "n") "m"))))))
(def MATH (l "x y s z" (-> "y" "s" (-> "x" (-> "y" "s") (-> "x" (-> "y" "s") (-> "y" "s" "z"))))))

(def predefs (->> (ns-publics *ns*)
                  (filter (fn [[sym _]] (every? #(or (not (Character/isAlphabetic (int %)))
                                                     (Character/isUpperCase ^Character %))
                                                (name sym))))
                  (map (fn [[sym var]]
                         [(str sym) (deref var)]))
                  (into {})))

(defn list-predefs []
  (doseq [[var val] predefs]
    (println var val)))

; ---------------------- TESTS ----------------------

(defmacro expect-assertion-error [form]
  `(try ~form
        (throw (Throwable. ~(str "Expected throw for form: " form)))
        (catch AssertionError _#
          "ok")))

(defn test-church-number [church-number expected-number]
  (let [church-number (conv/clojurize church-number)]
    (assert (and (vector? church-number)
                 (vector? (second church-number))) "Church number must be a lambda of 2 arguments.")

    (let [[s [z body]] church-number]
      (assert (not= s z) "Church number must have 2 unique arguments.")
      (loop [n 0
             body body]
        (cond (= body z) (assert (= n expected-number) (str "Church number does not have the expected value"
                                                            " (expected = " expected-number ", actual = " n ")."))
              (list? body) (let [[s2 sz-next] body]
                             (assert (= s s2) "Church number body mismatch.")
                             (recur (inc n) sz-next))
              :else (assert false "Church number body malformed, expected 'z' or an application."))))))

(defn test-boolean [lambda-boolean expected]
  (let [lambda-boolean (conv/clojurize lambda-boolean)]
    (assert (and (vector? lambda-boolean)
                 (vector? (second lambda-boolean))) "Boolean must be a lambda of 2 arguments.")
    (let [[t [f ret]] lambda-boolean]
      (if expected
        (assert (= t ret) "Expected true, got false")
        (assert (= f ret) "Expected false, got true")))))

(let [arg? (fn [x] (and (string? x) (= (count x) 1)))
      gensym? (fn [x] (and (symbol? x) (> (count (name x)) 1)))]
  (defn check-expr [form template context]
    (cond (string? form) (or (and (arg? template) (= form template))
                             (and (gensym? template) (= (context template) form)))
          (list? form) (and (list? template)
                            (= (count form) (count template) 2)
                            (every? identity (map #(check-expr %1 %2 context) form template)))
          (vector? form) (and (vector? template)
                              (= (count form) (count template) 2)
                              (let [[x1 body1] form
                                    [x2 body2] template]
                                (when-let [new-context (cond (and (arg? x2) (= x1 x2)) context
                                                             (gensym? x2) (if-let [in-context (context x2)]
                                                                            (and (= in-context x1) context) ; in context - must match
                                                                            (assoc context x2 x1)))]
                                  (check-expr body1 body2 new-context)))))))

(defmacro check-expression [form template]
  `(assert (check-expr ~form (quote ~template) {})))

(defn run-tests []
  (test-church-number (church-number 3) 3)
  (expect-assertion-error (test-church-number (church-number 3) 4))
  (test-church-number (eval/run-result (-> Y-COMB FACT 4)) 24)
  (test-church-number (eval/run-result (-> ADD 3 5)) 8)
  ; Xor
  (test-boolean (eval/run-result (-> XOR FALSE FALSE)) false)
  (test-boolean (eval/run-result (-> XOR TRUE FALSE)) true)
  (test-boolean (eval/run-result (-> XOR FALSE TRUE)) true)
  (test-boolean (eval/run-result (-> XOR TRUE TRUE)) false)
  (expect-assertion-error (test-boolean (eval/run-result (-> XOR TRUE TRUE)) true))
  ; Nand
  (test-boolean (eval/run-result (-> NAND FALSE FALSE)) true)
  (test-boolean (eval/run-result (-> NAND TRUE FALSE)) true)
  (test-boolean (eval/run-result (-> NAND FALSE TRUE)) true)
  (test-boolean (eval/run-result (-> NAND TRUE TRUE)) false)
  ; Mod2
  (test-church-number (eval/run-result (-> MOD2 42)) 0)
  (test-church-number (eval/run-result (-> MOD2 69)) 1)
  ; Recursion
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 0 0)) 1)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 0 1)) 2)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 0 2)) 4)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 1 0)) 2)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 1 1)) 4)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 1 2)) 8)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 2 0)) 4)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 2 1)) 8)
  (test-church-number (eval/run-result (-> Y-COMB RECURSION 2 2)) 16)
  ; Math
  (test-church-number (eval/run-result (-> MATH 0 0)) 0)
  (test-church-number (eval/run-result (-> MATH 0 1)) 2)
  (test-church-number (eval/run-result (-> MATH 2 5)) 30)
  (test-church-number (eval/run-result (-> MATH 3 2)) 16)
  ; Test some alpha conversion
  (check-expression (eval/run-result "a") "a")
  (expect-assertion-error (check-expression (eval/run-result "a") "b"))
  (check-expression (eval/eval-step (conv/clojurize (-> ["x" ["y" ["z" (-> "x" "z")]]] ["a" (-> "y" "z")])))
                    (["x" [_1 [_2 ("x" _2)]]] ["a" ("y" "z")]))
  (expect-assertion-error (check-expression (eval/eval-step (conv/clojurize (-> ["x" ["y" ["z" (-> "x" "z")]]] ["a" (-> "y" "z")])))
                                            (["x" [_1 [_2 ("x" _1)]]] ["a" ("y" "z")])))
  (expect-assertion-error (check-expression (eval/eval-step (conv/clojurize (-> ["x" ["y" ["a" (-> "x" "z")]]] ["a" (-> "y" "z")])))
                                            (["x" [_1 [_2 ("x" _2)]]] ["a" ("y" "z")]))))

(try (run-tests)
     (println "All tests passed.")
     (catch Throwable t
       (println "Fail:")
       (Thread/sleep 10)
       (.printStackTrace t)))
