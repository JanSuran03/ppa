(ns semestral.evaluator
  (:require [semestral.conversion :as conv]))

(defn beta-reduction [[x y :as form]]
  (cond (string? form) form                                 ; symbol - do nothing
        (vector? form) [x (beta-reduction y)]               ; lambda - try to reduce body
        (list? form) (cond (string? x) (list x (beta-reduction y)) ; call: target symbol - try to reduce argument
                           (list? x) (let [redx (beta-reduction x)] ; nested call - try to reduce, then try to reduce argument if no reduction is performed
                                       (if (= x redx)
                                         (list x (beta-reduction y))
                                         (list redx y)))
                           (vector? x) (let [[arg body] x]  ; pure lambda call - first, capture the lambda argument, then reduce body
                                         ((fn reduce-1 [form]
                                            (cond (string? form) (if (= form arg) y form) ; replace symbol if match
                                                  (vector? form) (if (= (first form) arg) form [(first form) (reduce-1 (second form))]) ; abort if argument is shadowed
                                                  (list? form) (list (reduce-1 (first form)) (reduce-1 (second form))))) ; reduce both branches of calls
                                          body)))))

; TODO: alpha conversion
; (λ t f . a) (λ a . a) <-
; (λ a . a) (λ t f . a) ->

(defn run-the-turing-fkin-machine [form]
  (let [form (conv/clojurize form)]
    ; (println form)
    (loop [form form
           i 1]
      ; (println (str "#" i))
      (let [new-form (beta-reduction form)]
        (if (= form new-form)
          form
          (do                                               ; (println new-form)
            ; (Thread/sleep 100)                              ; debug - prevent infinite loop
            (recur new-form (inc i))))))))

(defn run [form]
  (run-the-turing-fkin-machine form))
