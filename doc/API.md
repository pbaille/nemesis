# thetis — API Reference

Complete reference for the thetis polymorphic generics library.

**[← Back to README](../README.md)**

---

- [Core Concepts](#core-concepts)
  - [Type Keywords](#type-keywords)
  - [Type Hierarchy](#type-hierarchy)
  - [Precedence](#precedence)
  - [Protocol-per-Arity](#protocol-per-arity)
- [API](#api)
  - [`defg` — Define a Generic](#defg--define-a-generic)
  - [`generic+` — Extend a Generic](#generic--extend-a-generic)
  - [`type+` — Extend a Type](#type--extend-a-type)
  - [`thing` — Anonymous Implementation](#thing--anonymous-implementation)
  - [`fork` / `fork+` — Clone a Generic](#fork--fork----clone-a-generic)
  - [`deft` — Define a Record Type](#deft--define-a-record-type)
  - [`register-type` — Register a Custom Type](#register-type--register-a-custom-type)
  - [`thetis.types` — Type Registry Queries](#thetistypes--type-registry-queries)
  - [`defguard` — Define a Predicate Guard](#defguard--define-a-predicate-guard)
  - [`implements?` — Test Protocol Satisfaction](#implements--test-protocol-satisfaction)
- [Extension Modes](#extension-modes)
  - [`:sealed`](#sealed--no-extensions)
  - [`:extend`](#extend--add-only-no-overrides)
  - [`:refine` (default)](#refine--specialize-dont-override-default)
  - [`:override`](#override--full-override)
  - [Fork Mode Reset](#fork-mode-reset)
  - [Extension Ordering](#extension-ordering)
- [Predicate Guards](#predicate-guards)
- [ClojureScript](#clojurescript)
  - [Setup with shadow-cljs](#setup-with-shadow-cljs)
  - [Requiring in CLJS](#requiring-in-cljs)
- [Custom Types](#custom-types)
  - [Using `deft`](#using-deft)
  - [Using `register-type`](#using-register-type)
- [How It Works](#how-it-works)

---

## Core Concepts

### Type Keywords

Instead of host classes, you use keywords that map to platform-appropriate classes at compile time:

| Keyword     | CLJ class(es)                              | CLJS class(es)                                       |
|-------------|-------------------------------------------|------------------------------------------------------|
| `:vec`      | `IPersistentVector`                        | `PersistentVector`, `Subvec`, `BlackNode`, `RedNode` |
| `:map`      | `PersistentArrayMap`, `PersistentHashMap`  | `PersistentArrayMap`, `PersistentHashMap`, ...        |
| `:seq`      | `ISeq`                                     | `List`, `LazySeq`, `Range`, `Cons`, ...              |
| `:set`      | `IPersistentSet`                           | `PersistentHashSet`, `PersistentTreeSet`             |
| `:number`   | `Number`                                   | `number`                                             |
| `:string`   | `String`                                   | `string`                                             |
| `:keyword`  | `Keyword`                                  | `Keyword`                                            |
| `:symbol`   | `Symbol`                                   | `Symbol`                                             |
| `:function` | `Fn`                                       | `function`                                           |
| `:nil`      | `nil`                                      | `nil`                                                |

### Type Hierarchy

Types can be **aggregate** — one implementation covers an entire group.

Built-in groups:

| Group       | Includes                               |
|-------------|----------------------------------------|
| `:coll`     | `:seq`, `:vec`, `:set`, `:map`         |
| `:indexed`  | `:seq`, `:vec`                         |
| `:hashed`   | `:set`, `:map`                         |
| `:word`     | `:keyword`, `:symbol`, `:string`       |
| `:builtin`  | all base types                         |

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

Because `:vec` appears before `:coll`, vectors get their own implementation. Seqs (covered by `:coll` but not `:vec`, `:map`, or `:set`) get the catch-all.

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

Each arity is independently extensible — `generic+` can target one arity without affecting the others.

---

## API

### `defg` — Define a Generic

The core macro. Defines a generic function with type-dispatched implementations.

#### Single-arity

```clojure
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
```

The last expression without a type keyword is the **default** — it runs when no type matches.

#### Multi-arity

```clojure
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

Each arity body is independent — different types, different defaults.

#### With metadata

```clojure
(defg ^{:mode :sealed} frozen [x]
  :number (inc x)
  0)
```

Metadata on the name controls [extension mode](#extension-modes).

### `generic+` — Extend a Generic

Add implementations for new types or arities after the initial `defg`.

#### Single-arity extension

```clojure
(generic+ encode [x]
  :vec (str "[" (clojure.string/join " " (map encode x)) "]"))

(encode [1 :a "hi"])  ;=> "[1 a hi]"
```

#### Multi-arity extension

```clojure
(generic+ encode
  ([x]     :boolean (if x "true" "false"))
  ([x fmt] :boolean (case fmt :upper (clojure.string/upper-case (encode x)) (encode x))))
```

Extensions are subject to the generic's [extension mode](#extension-modes).

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

Useful for sentinel values, null objects, or test doubles.

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

Forks always reset to `:override` mode regardless of the parent's mode. See [Fork Mode Reset](#fork-mode-reset).

### `deft` — Define a Record Type

Creates a `defrecord`, a positional constructor, a cast generic, and registers a type keyword — all in one form.

```clojure
(deft rgb [r g b]
  :belongs-to [:map]
  (encode [c] (str (:r c) "," (:g c) "," (:b c))))
```

This generates:
- `(defrecord Rgb [r g b])` and `(def rgb ->Rgb)` — the record + positional constructor
- `(defg ->rgb [x] ...)` — a cast generic with a `:map` impl via `map->Rgb`
- Type registration as `:<current-ns>/rgb` — usable in `defg` / `generic+`

```clojure
(rgb 255 128 0)               ;=> #my.app.Rgb{:r 255 :g 128 :b 0}
(encode (rgb 255 128 0))      ;=> "255,128,0"
(->rgb {:r 10 :g 20 :b 30})   ;=> cast from map
```

The `:belongs-to` option specifies parent type groups. The cast generic is auto-extensible:

```clojure
(generic+ ->rgb [x]
  :vec (rgb (first x) (second x) (nth x 2)))

(->rgb [255 128 0])  ;=> #my.app.Rgb{:r 255 :g 128 :b 0}
```

### `register-type` — Register a Custom Type

Register existing classes as a new type keyword for use in generics:

```clojure
(register-type :my-type
  {:classes [my.package.MyClass]
   :groups [:coll]
   :impls [(encode [x] (.serialize x))]})
```

Options:
- `:classes` — vector of host classes to associate with this type keyword
- `:groups` — optional parent type groups this type belongs to
- `:impls` — optional generic implementations to install immediately

### `thetis.types` — Type Registry Queries

The `thetis.types` namespace exposes runtime access to the type registry — inspect the hierarchy, resolve type keywords to host classes, and generate predicates. Useful for tooling, introspection, and building on top of the type system.

```clojure
(require '[thetis.types :as tt])
```

#### Registry access

**`get-reg`** — Returns the current type registry map.

```clojure
(tt/get-reg)
;=> {:vec #{clojure.lang.IPersistentVector}
;    :coll #{:seq :vec :set :map}
;    :builtin #{:vec :map :seq :set :number ...}
;    ...}
```

**`get-type`** — Look up a single type keyword in the registry. Returns `nil` if not found.

```clojure
(tt/get-type :vec)   ;=> #{clojure.lang.IPersistentVector}
(tt/get-type :coll)  ;=> #{:seq :vec :set :map}
(tt/get-type :bogus) ;=> nil
```

**`all-types`** — Returns a set of all registered type keywords (built-in, aggregate, and custom).

```clojure
(tt/all-types)
;=> #{:vec :map :seq :set :number :string :keyword :symbol :function :nil
;     :coll :indexed :hashed :word :builtin ...}
```

#### Hierarchy queries

**`parents`** — Returns the immediate parent types of a keyword (types for which this keyword is a member).

```clojure
(tt/parents :vec)  ;=> (:coll :indexed :builtin)
(tt/parents :coll) ;=> (:builtin)
```

**`childs`** — Returns the immediate children of a type keyword (keywords that are members of it).

```clojure
(tt/childs :coll)  ;=> (:seq :vec :set :map)
(tt/childs :vec)   ;=> ()
```

**`childof`** — Returns truthy if `x` is a direct or indirect child of `y`.

```clojure
(tt/childof :vec :coll)    ;=> true
(tt/childof :vec :builtin) ;=> true
(tt/childof :coll :vec)    ;=> false
```

**`parentof`** — Returns truthy if `x` is a direct or indirect parent of `y`.

```clojure
(tt/parentof :coll :vec)   ;=> true
(tt/parentof :vec :coll)   ;=> false
```

**`classes`** — Resolves a type keyword to its concrete host classes, recursively expanding aggregate types.

```clojure
(tt/classes :vec)
;=> #{clojure.lang.IPersistentVector}

(tt/classes :coll)
;=> #{clojure.lang.ISeq clojure.lang.IPersistentVector
;     clojure.lang.IPersistentSet clojure.lang.IPersistentMap ...}
```

#### Type predicates

**`isa`** — Macro. Generates a type predicate for a keyword. Expands to efficient `instance?` checks at macro-expansion time — no runtime registry lookup.

```clojure
;; One-argument form — returns a predicate fn
(filter (tt/isa :vec) [[] {} () #{}])   ;=> ([] )
(filter (tt/isa :coll) [[] {} 42 "hi"]) ;=> ([] {})

;; Two-argument form — inline test, returns the value or nil
(tt/isa :number 42)    ;=> 42    (truthy)
(tt/isa :number "hi")  ;=> nil
(tt/isa :coll [1 2])   ;=> [1 2] (truthy)
```

Works with set literals too, for ad-hoc unions:

```clojure
((tt/isa #{:keyword :symbol}) :foo)   ;=> :foo
((tt/isa #{:keyword :symbol}) "str")  ;=> nil
```

**`symbolic-pred`** — Returns a quoted predicate form for a type keyword. Low-level; used internally by `isa` and the macro compiler. Takes an optional seed expression to produce an inline `let`-bound form.

```clojure
(tt/symbolic-pred :vec)
;=> (fn [G__1] (when (instance? clojure.lang.IPersistentVector G__1) G__1))
```

**`symbolic-pred-body`** — Returns just the condition expression for a type, without the wrapping `fn`. Useful when embedding type checks inside macro-generated code.

```clojure
(tt/symbolic-pred-body (tt/get-reg) :vec 'x)
;=> (clojure.core/instance? clojure.lang.IPersistentVector x)

(tt/symbolic-pred-body (tt/get-reg) :coll 'x)
;=> (clojure.core/or (instance? ...) (instance? ...) ...)
```

### `defguard` — Define a Predicate Guard

Define a predicate-refined subtype. See the full [Predicate Guards](#predicate-guards) section below.

```clojure
(defguard :positive
  :base :number
  :pred pos?)
```

### `implements?` — Test Protocol Satisfaction

Check whether a value satisfies one or more generics:

```clojure
(implements? x encode)             ; does x satisfy encode?
(implements? x encode combine)     ; does x satisfy both?
```

---

## Extension Modes

Control how a generic can be extended after creation. Set via metadata on the name in `defg`:

```clojure
(defg ^{:mode :sealed}   locked [x] ...)
(defg ^{:mode :extend}   open-ended [x] ...)
(defg ^{:mode :refine}   specializable [x] ...)   ; default
(defg ^{:mode :override} wide-open [x] ...)
```

### `:sealed` — No Extensions

The generic is closed. Nobody can add implementations after definition.

```clojure
(defg ^{:mode :sealed} frozen [x]
  :number (inc x)
  0)

(generic+ frozen [x] :string x)  ; ← compile error
```

Use for internal generics that should never be touched by consumers.

### `:extend` — Add Only, No Overrides

New types welcome. Overriding an existing type **or** specializing a parent type (e.g., adding `:vec` when `:coll` already covers it) are both blocked.

```clojure
(defg ^{:mode :extend} render [x]
  :coll (str "<list:" (count x) ">")
  (str x))

(generic+ render [x] :number (str "<num:" x ">"))  ; ✓ new type
(generic+ render [x] :vec    "<vec>")               ; ✗ :vec is child of :coll
(generic+ render [x] :coll   "<coll>")              ; ✗ override
```

Use when you want an open system but need to prevent breakage of existing behavior.

### `:refine` — Specialize, Don't Override *(default)*

Specialization is allowed — you can add `:vec` when `:coll` has an impl, because `:vec` is more specific. But directly overriding an existing implementation is blocked.

```clojure
(defg summarize [x]
  :coll (str (count x) " items")
  (str x))

(generic+ summarize [x] :vec (str "vec of " (count x)))  ; ✓ specialization
(generic+ summarize [x] :vec "something else")            ; ✗ :vec already has impl
(generic+ summarize [x] :number (str "n=" x))             ; ✓ new type
```

This is the default mode. It allows the type hierarchy to be progressively refined while preventing accidental overrides.

### `:override` — Full Override

No restrictions. Anything goes — any type can be added or replaced.

```clojure
(defg ^{:mode :override} debug [x]
  :vec (str "vec:" (count x))
  (pr-str x))

(generic+ debug [x] :vec (str "VEC[" (count x) "]"))  ; ✓ overrides existing
```

Use for development, debugging, or when you explicitly want a mutable dispatch table.

### Fork Mode Reset

When you `fork` a generic, the fork **always resets to `:override`** — even if the parent was `:sealed`. This is intentional: forks are your copy, you should have full control.

```clojure
(defg ^{:mode :sealed} frozen [x]
  :number (inc x)
  0)

(fork my-frozen frozen)
(generic+ my-frozen [x] :string x)  ; ✓ works — fork is :override
```

### Extension Ordering

When multiple namespaces extend the same type on an `:override` generic, the **namespace whose name sorts last alphabetically wins**. This ordering is deterministic and rebuild-safe — it ensures consistent behavior across hot reloads and incremental builds.

```clojure
;; ns: my.app.alpha
(generic+ debug [x] :vec "alpha impl")

;; ns: my.app.zeta  ← sorts after my.app.alpha
(generic+ debug [x] :vec "zeta impl")

(debug [1 2 3])  ;=> "zeta impl"  — zeta wins
```

This matters most in ClojureScript with incremental shadow-cljs builds, where namespaces may be recompiled in varying order. By making last-alphabetical-namespace the winner, the effective implementation is stable regardless of build order.

> **Tip:** If you need a specific namespace to "own" a particular override, name it so it sorts last among the competing namespaces — or use `fork` to take a private copy of the generic instead.

---

## Predicate Guards

Guards extend the type system with runtime predicates. They're checked **before** protocol dispatch — first matching guard wins, then falls through to normal type-based dispatch.

### Defining guards

```clojure
(defguard :positive
  :base :number
  :pred pos?)

(defguard :blank
  :base :string
  :pred clojure.string/blank?)
```

- `:base` — the parent type keyword. The guard's predicate is only checked for values of this type.
- `:pred` — a predicate function. If it returns truthy, the guard matches.

### Using guards in generics

Guards are used as type keywords in `defg` and `generic+`:

```clojure
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

### Dispatch order

1. Guards matching the value's type are checked first (in definition order)
2. If no guard matches, normal protocol dispatch runs

### Extension mode integration

A guard is treated as a **child** of its base type in the extension mode system:

- Under `:refine` (default): adding `:positive` when `:number` has an impl is **specialization** (allowed)
- Under `:extend`: adding `:positive` when `:number` has an impl is **blocked** (child of existing type)
- Under `:override`: always allowed

### Adding guards to existing generics

```clojure
(defguard :non-empty-vec
  :base :vec
  :pred seq)

(generic+ validate [x]
  :non-empty-vec x
  :vec           (throw (ex-info "Empty vector" {})))
```

### Guards across forks

Guards work seamlessly across forks and multi-arity generics. A forked generic inherits all guard implementations from the parent.

---

## ClojureScript

Thetis works on both CLJ and CLJS. All macros run on the JVM and emit CLJS-compatible code. Type keywords resolve to the correct CLJS classes at compile time.

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

**Configuration explained:**

- **`:cache-blockers #{thetis.core}`** — Forces recompilation of the thetis core namespace. Required because macro expansion depends on mutable compile-time state.

- **`thetis.core/prepare-state!`** — Incremental build hook. Resets the namespace preparation tracker so that recompiled namespaces re-register their generics without losing state from unchanged namespaces. This is the recommended hook for development.

- **`thetis.core/reset-state!`** — Full reset hook. Use instead of `prepare-state!` for a clean rebuild every time. Slower but simpler — useful if you encounter stale state issues.

### Requiring in CLJS

```clojure
(ns my.app
  (:require [thetis.core :as g
             :refer [defg generic+ fork]
             :include-macros true]))
```

The `:include-macros true` tells ClojureScript to load the JVM-side macros from `thetis.core`.

---

## Custom Types

### Using `deft`

The easiest way to introduce a new type with full generic integration:

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

---

## How It Works

1. **`defg`** parses the body into typed cases, generates one protocol per arity, emits a `defn` wrapper + `extend` calls for each type.

2. **Type resolution** happens at macro-expansion time: `:vec` → registry lookup → `#{IPersistentVector}` (CLJ) or `#{PersistentVector, Subvec, ...}` (CLJS). No runtime type checking.

3. **Aggregate types** resolve recursively: `:coll` → `#{:seq :vec :set :map}` → all leaf classes for the current platform.

4. **Runtime `prototypes` atom** stores `{type → generic → arity → fn}`. Protocol extensions read from it via `get-in` + `deref`. This enables `thing`, `fork`, and runtime introspection.

5. **Guards** add two-phase dispatch: the generic wrapper checks the `guard-impls` atom (a cond-chain of predicate checks) → if no guard matches, falls through to normal protocol dispatch.

---

**[← Back to README](../README.md)**
