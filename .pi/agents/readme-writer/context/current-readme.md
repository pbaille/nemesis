# thetis

Cross-platform polymorphic generics for Clojure and ClojureScript.

Thetis replaces `defprotocol` / `extend-type` / `extend-protocol` with a unified macro layer that uses **type keywords** (`:vec`, `:map`, `:seq`, `:coll`) instead of host-platform classes. Write once, run on both CLJ and CLJS — no reader conditionals, no duplicated implementations for CLJS concrete classes.

Under the hood, thetis compiles to protocols. Dispatch is zero-overhead on CLJ (JVM interface calls) and near-zero on CLJS (prototype lookup).

## Quick Start

Add to `deps.edn`:

```clojure
;; From Clojars (when published)
{:deps {pbaille/thetis {:mvn/version "0.2.0"}}}

;; Or from local checkout
{:deps {pbaille/thetis {:local/root "/path/to/thetis"}}}
```

```clojure
(ns my.app
  (:require [thetis.core :refer [defg generic+] :include-macros true]))

;; Define a generic function
(defg combine [a b]
  :vec    (into a b)
  :map    (merge a b)
  :set    (into a b)
  :seq    (concat a b)
  :string (str a b)
  (throw (ex-info "Cannot combine" {:a a :b b})))

(combine [1 2] [3 4])       ;=> [1 2 3 4]
(combine {:a 1} {:b 2})     ;=> {:a 1 :b 2}
(combine '(1 2) '(3 4))     ;=> (1 2 3 4)
(combine "hello " "world")  ;=> "hello world"

;; Extend later — new types, no touching the original definition
(generic+ combine [a b]
  :number (+ a b))

(combine 10 20)  ;=> 30
```

## Why thetis?

Clojure's protocol system is powerful but has friction:

- **CLJS requires concrete types**: Extending a protocol to `PersistentVector`, `Subvec`, `BlackNode`, `RedNode`... for what's conceptually "a vector". Thetis: just write `:vec`.
- **No type grouping**: Want one implementation for all collections? With protocols you list every class. Thetis: write `:coll`, it covers `:vec`, `:map`, `:set`, `:seq`.
- **Multi-arity protocols are awkward**: One protocol per arity, naming the methods yourself. Thetis: write a multi-arity `defg`, it generates the protocols behind the scenes.
- **No safety controls**: Anyone can extend anyone's protocol however they want. Thetis: extension modes (`:sealed`, `:extend`, `:refine`, `:override`) give you control.
- **No cross-platform story**: Reader conditionals everywhere. Thetis: type keywords resolve to the right classes per platform at compile time.

## Core Concepts

### Type Keywords

Instead of host classes, you use keywords that map to platform-appropriate classes at compile time:

| Keyword     | CLJ class(es)                    | CLJS class(es)                        |
|-------------|----------------------------------|---------------------------------------|
| `:vec`      | `IPersistentVector`              | `PersistentVector`, `Subvec`, ...     |
| `:map`      | `PersistentArrayMap`, `PersistentHashMap` | `PersistentArrayMap`, `PersistentHashMap`, ... |
| `:seq`      | `ISeq`                           | `List`, `LazySeq`, `Range`, `Cons`, ... |
| `:set`      | `IPersistentSet`                 | `PersistentHashSet`, `PersistentTreeSet` |
| `:number`   | `Number`                         | `number`                              |
| `:string`   | `String`                         | `string`                              |
| `:keyword`  | `Keyword`                        | `Keyword`                             |
| `:symbol`   | `Symbol`                         | `Symbol`                              |
| `:function` | `Fn`                             | `function`                            |
| `:nil`      | `nil`                            | `nil`                                 |

### Type Hierarchy

Types can be **aggregate** — `:coll` covers `:seq`, `:vec`, `:set`, and `:map`. One implementation for `:coll` extends to all four.

Built-in groups:

- `:coll` → `:seq :vec :set :map`
- `:indexed` → `:seq :vec`
- `:hashed` → `:set :map`
- `:word` → `:keyword :symbol :string`
- `:builtin` → all base types

You can also use **set literals** for ad-hoc unions: `#{:keyword :symbol}`.

### Precedence

Implementation order matters — like `cond`, first match wins:

```clojure
(defg empty-of [x]
  :vec []
  :map {}
  :set #{}
  :coll ()    ; catches remaining colls (seqs)
  nil)

(empty-of [1 2])    ;=> []
(empty-of {:a 1})   ;=> {}
(empty-of '(1 2))   ;=> ()
(empty-of 42)       ;=> nil
```

### Protocol-per-Arity

A multi-arity generic generates one protocol per arity. You can extend a single arity independently — un-extended arities fall through to defaults.

```clojure
(defg stringify
  ([x]
   :vec    (str "[" (clojure.string/join " " (map stringify x)) "]")
   :map    (str "{" (clojure.string/join ", " (map (fn [[k v]] (str (stringify k) " " (stringify v))) x)) "}")
   :number (str x)
   :keyword (name x)
   (pr-str x))
  ([x sep]
   :coll (clojure.string/join sep (map stringify x))
   (stringify x)))

(stringify [1 :a 2])           ;=> "[1 a 2]"
(stringify [1 :a 2] " | ")    ;=> "1 | a | 2"
```

## API Reference

### `defg` — Define a Generic

```clojure
;; Single-arity
(defg fmap [x f]
  :vec  (mapv f x)
  :map  (into {} (map (fn [[k v]] [k (f v)])) x)
  :set  (into #{} (map f) x)
  :seq  (map f x)
  :nil  nil
  (f x))

(fmap [1 2 3] inc)        ;=> [2 3 4]
(fmap {:a 1 :b 2} str)    ;=> {:a "1" :b "2"}
(fmap #{1 2 3} #(* % %))  ;=> #{1 4 9}

;; Multi-arity
(defg encode
  ([x]
   :string  x
   :number  (str x)
   :keyword (name x)
   :nil     ""
   (pr-str x))
  ([x fmt]
   :string (case fmt :upper (clojure.string/upper-case x) x)
   :coll   (case fmt :json (str "[" (clojure.string/join "," (map encode x)) "]") (encode x))
   (encode x)))

(encode "hello" :upper)             ;=> "HELLO"
(encode [1 :a "hi"] :json)          ;=> "[1,a,hi]"
```

### `generic+` — Extend a Generic

Add implementations for new types or arities:

```clojure
(generic+ encode [x]
  :vec (str "[" (clojure.string/join " " (map encode x)) "]"))

(encode [1 :a "hi"])  ;=> "[1 a hi]"
```

Extend multiple arities at once:

```clojure
(generic+ encode
  ([x]     :boolean (if x "true" "false"))
  ([x fmt] :boolean (case fmt :upper (clojure.string/upper-case (encode x)) (encode x))))
```

### `type+` — Extend a Type

Implement several generics for one type at once (like `extend-type`):

```clojure
(type+ :function
  (encode [f]  "#<fn>")
  (fmap [f g]  (comp g f)))

(encode inc)         ;=> "#<fn>"
(fmap inc str)       ;=> a function: (comp str inc)
((fmap inc str) 42)  ;=> "43"
```

### `thing` — Anonymous Implementation

Like `reify` but for generics — create ad-hoc objects that satisfy them:

```clojure
(def empty-source
  (thing
    (encode [_]     "∅")
    (fmap [_ f]     empty-source)
    (combine [_ x]  x)))

(encode empty-source)           ;=> "∅"
(combine empty-source [1 2])    ;=> [1 2]
(fmap empty-source inc)         ;=> empty-source — absorbs transforms
```

### `fork` / `fork+` — Clone a Generic

Create an independent copy. The fork inherits all implementations but is fully isolated — extending one does not affect the other.

```clojure
;; Suppose `encode` comes from a library.
;; You want your own version that handles domain types differently.

(fork my-encode encode)

(generic+ my-encode [x]
  :keyword (str ":" (name x)))  ; different behavior for keywords

(my-encode :foo)  ;=> ":foo"
(encode :foo)     ;=> "foo"     — original unchanged
```

`fork+` combines forking and extending in one step:

```clojure
(fork+ my-encode encode
  [x] :keyword (str ":" (name x)))
```

Forks always reset to `:override` mode regardless of the parent's mode.

### `deft` — Define a Record Type

Creates a `defrecord`, a positional constructor, a cast generic, and registers a type keyword.

```clojure
(deft rgb [r g b]
  :belongs-to [:map]
  (encode [c] (str (:r c) "," (:g c) "," (:b c))))
```

This generates:
- `(defrecord Rgb [r g b])` and `(def rgb ->Rgb)`
- `(defg ->rgb [x] ...)` — cast generic with a `:map` impl via `map->Rgb`
- Type registration as `:<current-ns>/rgb`

```clojure
(rgb 255 128 0)             ;=> #my.app.Rgb{:r 255 :g 128 :b 0}
(encode (rgb 255 128 0))    ;=> "255,128,0"
(->rgb {:r 10 :g 20 :b 30}) ;=> cast from map
```

### `register-type` — Register a Custom Type

Register existing classes as a new type keyword:

```clojure
(register-type :my-type
  {:classes [my.package.MyClass]
   :groups [:coll]
   :impls [(encode [x] (.serialize x))]})
```

### `defguard` — Define a Predicate Guard

Guards are predicate-refined subtypes that dispatch at runtime. See [Predicate Guards](#predicate-guards).

### `implements?` — Test Protocol Satisfaction

```clojure
(implements? x encode)             ; does x satisfy encode?
(implements? x encode combine)     ; does x satisfy both?
```

## Extension Modes

Control how a generic can be extended after creation. Set via metadata:

```clojure
(defg ^{:mode :sealed}   locked [x] ...)
(defg ^{:mode :extend}   open-ended [x] ...)
(defg ^{:mode :refine}   specializable [x] ...)   ; default
(defg ^{:mode :override} wide-open [x] ...)
```

### `:sealed` — No Extensions

The generic is closed. Nobody can add implementations.

```clojure
(defg ^{:mode :sealed} frozen [x]
  :number (inc x)
  0)

(generic+ frozen [x] :string x)  ; compile error
```

### `:extend` — Add Only, No Overrides

New types welcome. Overriding an existing type or specializing a parent type (e.g., adding `:vec` when `:coll` already covers it) are both blocked.

```clojure
(defg ^{:mode :extend} render [x]
  :coll (str "<list:" (count x) ">")
  (str x))

(generic+ render [x] :number (str "<num:" x ">"))         ; ok — new type
(generic+ render [x] :vec    "<vec>")                      ; error — :vec is child of :coll
(generic+ render [x] :coll   "<coll>")                     ; error — override
```

### `:refine` — Specialize, Don't Override *(default)*

Specialization is allowed — you can add `:vec` when `:coll` has an impl, because `:vec` is more specific. But directly overriding an existing implementation is blocked.

```clojure
(defg summarize [x]
  :coll (str (count x) " items")
  (str x))

(generic+ summarize [x] :vec (str "vec of " (count x)))  ; ok — specialization
(generic+ summarize [x] :vec "something else")            ; error — :vec already has impl
(generic+ summarize [x] :number (str "n=" x))             ; ok — new type
```

### `:override` — Full Override

No restrictions. Anything goes.

```clojure
(defg ^{:mode :override} debug [x]
  :vec (str "vec:" (count x))
  (pr-str x))

(generic+ debug [x] :vec (str "VEC[" (count x) "]"))  ; ok — overrides existing
```

### Fork Mode Reset

When you `fork` a generic, the fork always resets to `:override` — even if the parent was `:sealed`. Forks are fully customizable.

## Predicate Guards

Guards extend the type system with runtime predicates. They're checked before protocol dispatch — first matching guard wins, then falls through to normal dispatch.

```clojure
(defguard :positive
  :base :number
  :pred pos?)

(defguard :blank
  :base :string
  :pred clojure.string/blank?)

(defg validate [x]
  :positive  x
  :blank     nil
  :number    (throw (ex-info "Must be positive" {:n x}))
  :string    x
  x)

(validate 42)       ;=> 42
(validate -3)       ;=> throws "Must be positive"
(validate "hello")  ;=> "hello"
(validate "")       ;=> nil
(validate [1 2])    ;=> [1 2]
```

Guards integrate with extension modes — a guard is treated as a child of its base type. Under `:refine`, adding `:positive` when `:number` has an impl is specialization (allowed). Under `:extend`, it's blocked.

Guards can be added via `generic+`:

```clojure
(defguard :non-empty-vec
  :base :vec
  :pred seq)

(generic+ validate [x]
  :non-empty-vec x
  :vec           (throw (ex-info "Empty vector" {})))
```

Guards work across forks and multi-arity generics.

## ClojureScript

Thetis works on both CLJ and CLJS. All macros are JVM-side and emit CLJS-compatible code.

### Setup with shadow-cljs

`shadow-cljs.edn`:

```clojure
{:deps {:aliases [:cljs]}
 :cache-blockers #{thetis.core}
 :builds
 {:main
  {:target :browser
   :build-hooks [(thetis.core/prepare-state!)]
   :modules {:main {:entries [my.app]}}}}}
```

**Key points:**

- **`:cache-blockers #{thetis.core}`** — Forces recompilation of this namespace (required for macro state).
- **`thetis.core/prepare-state!`** — Incremental build hook. Resets the namespace preparation tracker so recompiled namespaces re-register without losing state from unchanged namespaces.
- **`thetis.core/reset-state!`** — Full reset hook. Use instead of `prepare-state!` for a clean rebuild every time (slower but simpler).

### Requiring in CLJS

```clojure
(ns my.app
  (:require [thetis.core :as g
             :refer [defg generic+ fork]
             :include-macros true]))
```

The `:include-macros true` tells ClojureScript to load the JVM-side macros.

## Custom Types

### Using `deft`

The easiest way to introduce a new type:

```clojure
(deft point [x y]
  :belongs-to [:map]
  (encode [p]    (str "(" (:x p) ", " (:y p) ")"))
  (fmap [p f]    (point (f (:x p)) (f (:y p))))
  (combine [a b] (point (+ (:x a) (:x b)) (+ (:y a) (:y b)))))

(encode (point 3 4))                    ;=> "(3, 4)"
(fmap (point 1 2) #(* % 10))            ;=> (point 10 20)
(combine (point 1 2) (point 3 4))       ;=> (point 4 6)

;; Cast from map (enabled by :belongs-to [:map])
(->point {:x 10 :y 20})  ;=> (point 10 20)

;; Extend the cast generic
(generic+ ->point [x]
  :vec (point (first x) (second x)))

(->point [5 6])  ;=> (point 5 6)
```

### Using `register-type`

For existing classes that aren't records:

```clojure
(register-type :my-type
  {:classes [my.package.MyClass]
   :groups [:coll]
   :impls [(encode [x] (.serialize x))]})
```

## How It Works

1. **`defg`** parses the body into typed cases, generates one protocol per arity, emits a `defn` wrapper + `extend` calls.
2. **Type resolution** happens at macro-expansion time: `:vec` → registry lookup → `#{IPersistentVector}` (CLJ) or `#{PersistentVector, Subvec, ...}` (CLJS).
3. **Aggregate types** resolve recursively: `:coll` → `#{:seq :vec :set :map}` → all leaf classes.
4. **Runtime `prototypes` atom** stores `{type → generic → arity → fn}`. Protocol extensions read from it via `get-in` + `deref`.
5. **Guards** add two-phase dispatch: check the `guard-impls` atom (cond-chain) → fall through to protocol dispatch.

## Development

```bash
# Run CLJ tests
clj -M -e "(require 'thetis.tries.one 'thetis.tries.two 'thetis.tries.three 'thetis.tries.four 'thetis.tries.five)"

# Run CLJS tests (shadow-cljs)
npx shadow-cljs compile main

# Build JAR
clj -T:build jar

# Install to local .m2
clj -T:build install

# Deploy to Clojars (requires CLOJARS_USERNAME and CLOJARS_PASSWORD env vars)
clj -T:build deploy
```

## License

MIT
