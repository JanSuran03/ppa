(ns semestral.evaluator
  (:require [semestral.conversion :as conv]))

(def ^:dynamic *step-by-step-eval* true)
(def ^:dynamic *interval* 200)

(defn find-free-variables
  "Returns a set of free variables in the form."
  [form form-bound-vars]
  ((fn find-inner [form bound-vars free-vars]
     (cond (vector? form) (find-inner (second form) (conj bound-vars (first form)) free-vars)
           (list? form) (->> free-vars
                             (find-inner (first form) bound-vars)
                             (find-inner (second form) bound-vars))
           :else (if (bound-vars form)
                   free-vars
                   (conj free-vars form))))
   form form-bound-vars #{}))

(let [counter (volatile! 0)]
  (defn mini-gensym []
    (str "G_" (vswap! counter inc))))

(defn rename-bound-variables
  "Given a form and a set of colliding variable names, renames bound variables in the form iff they are bound."
  [form collisions]
  (let [mappings (apply hash-map (interleave collisions (repeatedly (comp str mini-gensym))))]
    ((fn rename-vars [form mappings-ctx]                    ; keep only bound mappings
       (cond (list? form) (list (rename-vars (first form) mappings-ctx)
                                (rename-vars (second form) mappings-ctx))
             (vector? form) [(mappings (first form) (first form)) ; always rename
                             (rename-vars (second form) (conj mappings-ctx (find mappings (first form))))] ; propagate signal to rename
             :else (mappings-ctx form form)))
     form {})))

(defn alpha-conversion
  "Renames bound variables in the form to unique names if necessary."
  [form argument argument-bound-vars]
  (let [free-variables (find-free-variables argument argument-bound-vars)]
    (rename-bound-variables form free-variables)))

(defn beta-reduction
  "Performs a beta reduction on the form. If an alpha conversion needs to be performed, instead performs
  the alpha conversion and returns, potentially performing beta reduction in the next call."
  [form]
  ((fn beta-reduction1 [[x y :as form] bound-vars]
     (cond (string? form) form                              ; symbol - do nothing
           (vector? form) [x (beta-reduction1 y (conj bound-vars x))]            ; lambda - try to reduce body
           (list? form) (cond (string? x) (list x (beta-reduction1 y bound-vars)) ; call: target symbol - try to reduce argument
                              (list? x) (let [redx (beta-reduction1 x bound-vars)] ; nested call - try to reduce, then try to reduce argument if no reduction is performed
                                          (if (= x redx)
                                            (list x (beta-reduction1 y bound-vars))
                                            (list redx y)))
                              (vector? x) (let [alpha-converted-x (alpha-conversion x y bound-vars)] ; pure lambda call - first, capture the lambda argument, then reduce body
                                            (let [[arg body] alpha-converted-x]
                                              (if (= x alpha-converted-x)
                                                ((fn reduce-1 [form]
                                                   (cond (string? form) (if (= form arg) y form) ; replace symbol if match
                                                         (vector? form) (if (= (first form) arg) form [(first form) (reduce-1 (second form))]) ; abort if argument is shadowed
                                                         (list? form) (list (reduce-1 (first form)) (reduce-1 (second form))))) ; reduce both branches of calls
                                                 body)
                                                (list alpha-converted-x y)))))))
   form #{})) ; do one step at a time - only alpha conversion.

(defn run-the-turing-fkin-machine
  "Runs the ultra turbo 69420 core Turing machine."
  [form]
  (let [form (conv/clojurize form)]
    (if *step-by-step-eval* (println (str "#0: " form)))
    (loop [form form
           i 1]
      (let [new-form (beta-reduction form)]
        (if (= form new-form)
          form
          (do (if *step-by-step-eval* (println (str "#" i ": " new-form)))
              (if (> *interval* 0) (Thread/sleep ^Long *interval*))
              (recur new-form (inc i))))))))

(defn run
  "Some might say the 'only correct name' of this function is too long and hard to type without linters."
  [form]
  (run-the-turing-fkin-machine form))
