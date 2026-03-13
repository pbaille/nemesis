(ns thetis.state
  (:refer-clojure :exclude [reset! swap! get])
  (:require [clojure.core :as c]
            [thetis.utils.expansion :as exp]
            [thetis.types.core :as types]
            [thetis.types.data :as type-data]))

(def state0
  (let [base {:functions {}
              :types {}
              :type-state {:base {} :contributions {}}
              :guards {}
              :guard-state {:base {} :contributions {}}}]
    {:clj base
     :cljs base}))

(def state (atom state0))

;; === Expansion state ===
;;
;; Dynamic var bound during macro expansion.
;; Pure thetis modules never read this — they receive expansion context
;; as part of the compiler value {:expansion {:env ... :form ...}}.
;; Only thetis.state and thetis.core touch this var.

(defonce ^:dynamic *expansion-state*
  {:env nil :form nil})

(defn env [] (:env *expansion-state*))
(defn form [] (:form *expansion-state*))
(defn cljs? [] (exp/cljs? *expansion-state*))

;; === State accessors ===

(defn clj-state [] (:clj @state))
(defn cljs-state [] (:cljs @state))

(defn current []
  (if (cljs?) (cljs-state) (clj-state)))

(defn get
  ([] (current))
  ([k] (c/get (current) k)))

(defn compilation-target []
  (if (cljs?) :cljs :clj))

;; === Expansion macros ===

(defmacro expanding
  "Bind expansion state from the macro's &env and &form."
  [& body]
  `(binding [*expansion-state* {:env ~'&env :form ~'&form}]
     ~@body))

;; === State mutations ===

(defn swap! [f & args]
  (c/swap! state update (compilation-target) #(apply f % args)))

(defn reset!
  ([] (reset! state0))
  ([x] (clojure.core/reset! state x)))

;; === Namespace preparation for incremental builds ===
;;
;; Tracks which namespaces have been "prepared" in the current build.
;; The first macro expansion in a namespace triggers cleanup of that
;; namespace's old contributions, ensuring idempotent re-registration.

(def ^:private prepared-namespaces (atom #{}))

(defn ns-prepared?
  "Has this namespace already been prepared in the current build pass?"
  [ns-str]
  (contains? @prepared-namespaces ns-str))

(defn mark-ns-prepared!
  "Mark a namespace as prepared for this build pass."
  [ns-str]
  (c/swap! prepared-namespaces conj ns-str))

(defn reset-prepared!
  "Reset the set of prepared namespaces (called at build start)."
  []
  (c/reset! prepared-namespaces #{}))

;; === Symbol resolution ===

(defn qualify-symbol
  "Resolve a symbol to its fully-qualified name using the current expansion context."
  [x]
  (exp/qualify-symbol *expansion-state* x))

;; === Type initialization ===

(defn init-types!
  "Populate the type registries for both CLJ and CLJS platforms.
   Called at load time and available for reset hooks."
  []
  (let [clj-types (merge type-data/clj-base-types type-data/groups)
        cljs-types (merge type-data/cljs-base-types type-data/groups)]
    (c/swap! state
             (fn [s]
               (-> s
                   (assoc-in [:clj :types] clj-types)
                   (assoc-in [:clj :type-state] (types/make-type-state clj-types))
                   (assoc-in [:cljs :types] cljs-types)
                   (assoc-in [:cljs :type-state] (types/make-type-state cljs-types)))))))

(init-types!)


