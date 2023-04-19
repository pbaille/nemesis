(ns poly.functions.registry
  (:require [poly.utils.misc :as u]
            [poly.functions.spec :as spec]))

(defn get! [reg name]
  (or (get reg name)
      (and (u/error "Cannot find spec " (pr-str name) "\nin\n" (keys reg)))))

(defn add-spec [reg spec]
  (assoc-in reg (:fullname spec) spec))

(defn extend-spec [reg extension-spec]
  (add-spec reg
            (spec/merge-cases (get! reg (:fullname extension-spec))
                              extension-spec)))

(defn clone-spec [reg original-name new-name]
  (add-spec reg
            (spec/clone (get! reg original-name)
                        new-name)))

(defn implementers-map [reg]
  (u/$vals reg spec/implementers))

(defn get-class-cases
  "get all generics implementations for the given class"
  [reg class]
  (mapv (fn [spec]
          {:spec spec
           :cases (filter (fn [case]
                            (= class (:class case)))
                          (spec/class-extensions spec))})
        (vals reg)))
