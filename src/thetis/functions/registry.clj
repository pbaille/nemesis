(ns thetis.functions.registry
  "Function spec registry — a map of {fullname → spec}.
   Operations: add, extend, clone, remove namespace contributions."
  (:require [thetis.utils.misc :as u]
            [thetis.functions.spec :as spec]))

(defn get! [reg name]
  (or (get reg name)
      (and (u/error "Cannot find spec " (pr-str name) "\nin\n" (keys reg)))))

(defn add-spec
  "Register a spec. Initializes structured case storage if not already present."
  [reg spec]
  (let [spec (if (:declaration-cases spec)
               spec
               (spec/init-spec spec))]
    (assoc reg (:fullname spec) spec)))

(defn extend-spec
  "Extend a spec with new cases, keyed by namespace for idempotent rebuilds.
   extension-ns is required."
  [reg extension-spec extension-ns & {:keys [type-registry guards]}]
  (add-spec reg
            (spec/merge-cases (get! reg (:fullname extension-spec))
                              extension-spec
                              extension-ns
                              :type-registry type-registry
                              :guards guards)))

(defn clone-spec [reg original-name new-name]
  (add-spec reg
            (spec/clone (get! reg original-name)
                        new-name)))

(defn remove-ns-contributions
  "Remove all contributions from a given namespace.
   - Removes specs whose :ns matches the namespace
   - Removes extension-cases entries keyed by the namespace
   - Recomputes :cases for affected specs"
  [reg ns-str]
  (reduce-kv
   (fn [acc fullname spec]
     (if (= ns-str (str (:ns spec)))
       ;; Spec was defined in this ns — remove entirely
       acc
       ;; Check for extension contributions from this ns
       (if (get-in spec [:extension-cases ns-str])
         (let [updated (update spec :extension-cases dissoc ns-str)]
           (assoc acc fullname (assoc updated :cases (spec/effective-cases updated))))
         (assoc acc fullname spec))))
   {} reg))

(defn implementers-map [reg]
  (u/$vals reg spec/implementers))

(defn get-class-cases
  "get all generics implementations for the given class.
   type-registry is needed to resolve type keywords to concrete classes."
  [reg type-registry class]
  (mapv (fn [spec]
          {:spec spec
           :cases (filter (fn [case]
                            (= class (:class case)))
                          (spec/class-extensions spec type-registry))})
        (vals reg)))
