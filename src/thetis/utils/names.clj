(ns thetis.utils.names
  "Naming conventions for protocol and method symbols.
   Each generic `foo` gets protocol prefix `Ifoo` and method prefix `p_foo`,
   arified with the arity count (e.g., `Ifoo_2`, `p_foo_2`)."
  (:require [thetis.utils.misc :as u]))

(defn name_derive
  "Derive all naming components from a generic name symbol.
   Returns {:ns, :name, :protocol-prefix, :method-prefix, :fullname}."
  [n]
  (let [ns-str (or (namespace n) (str *ns*))
        ns (symbol ns-str)
        name-str (name n)
        name (symbol name-str)]
    {:ns ns
     :name name
     :protocol-prefix (u/sym 'I (munge name))
     :method-prefix (u/sym 'p_ (munge name))
     :fullname (symbol ns-str name-str)}))

(defn name_arify
  "Append an arity suffix to a name prefix: `Ifoo` + 2 → `Ifoo_2`."
  [n a]
  (u/sym n '_ (str a)))
