# thetis

**Polymorphic generics for Clojure and ClojureScript — write once, dispatch everywhere.**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Clojure 1.11+](https://img.shields.io/badge/Clojure-1.11+-green.svg)](https://clojure.org)
[![ClojureScript](https://img.shields.io/badge/ClojureScript-compatible-green.svg)](https://clojurescript.org)

```clojure
(require '[thetis.core :refer [defg generic+]])

(defg greet [x]
  :string  (str "Hello, " x "!")
  :keyword (str "Hello, " (name x) "!")
  :vec     (mapv greet x)
  "Hello, stranger!")

(greet "world")        ;=> "Hello, world!"
(greet :alice)         ;=> "Hello, alice!"
(greet ["Bob" :Carol]) ;=> ["Hello, Bob!" "Hello, Carol!"]
(greet 42)             ;=> "Hello, stranger!"

;; Extend it later — no touching the original
(generic+ greet [x]
  :number (str "Hello, #" x "!"))

(greet 42)             ;=> "Hello, #42!"
```

## Why thetis?

Protocols are powerful but painful. Here's what thetis fixes:

<table>
<tr><th>Raw protocols</th><th>With thetis</th></tr>
<tr>
<td>

```clojure
;; CLJS: extend to every concrete type
(extend-type PersistentVector IShow (-show [v] ...))
(extend-type Subvec          IShow (-show [v] ...))
(extend-type BlackNode       IShow (-show [v] ...))
(extend-type RedNode         IShow (-show [v] ...))
;; ... and you still missed some
```

</td>
<td>

```clojure
;; One keyword. Both platforms.
(defg show [x]
  :vec (str x))
```

</td>
</tr>
</table>

- **`:vec` instead of 5 class names** — Type keywords resolve to the right host classes at compile time. Works on CLJ and CLJS identically.
- **`:coll` covers everything** — Aggregate types (`:coll` → `:vec :map :set :seq`) give you one impl for all collections.
- **Multi-arity just works** — `defg` with N arities generates N protocols automatically. No naming, no ceremony.
- **Extension safety** — Four modes (`:sealed`, `:extend`, `:refine`, `:override`) control who can change what. Default is `:refine` — specialize, don't break.
- **Zero overhead on CLJ** — Compiles to protocol dispatch (JVM interface calls). No runtime reflection.

## Features

| Feature | Description |
|---------|-------------|
| [**Type keywords & hierarchy**](doc/API.md#type-keywords) | `:vec`, `:map`, `:coll` — platform-independent type dispatch |
| [**Extension modes**](doc/API.md#extension-modes) | `:sealed` / `:extend` / `:refine` / `:override` — control extensibility |
| [**Predicate guards**](doc/API.md#predicate-guards) | `defguard` — runtime predicate refinement (`:positive`, `:blank`, etc.) |
| [**Fork**](doc/API.md#fork--fork----clone-a-generic) | Clone a generic, customize independently. Full isolation. |
| [**`deft`**](doc/API.md#deft--define-a-record-type) | Define record types with auto-registration + cast generics |
| [**`thing`**](doc/API.md#thing--anonymous-implementation) | Anonymous objects that satisfy generics (like `reify` for generics) |
| [**`type+`**](doc/API.md#type--extend-a-type) | Extend multiple generics for one type at once |
| [**ClojureScript**](doc/API.md#clojurescript) | Full support via shadow-cljs with incremental build hooks |

## Quick Start

Add to `deps.edn` (use latest SHA from [master](https://github.com/pbaille/thetis)):

```clojure
{:deps {io.github.pbaille/thetis {:git/sha "LATEST_SHA"}}}
```

```clojure
(ns my.app
  (:require [thetis.core :refer [defg generic+ fork type+ thing]
             :include-macros true]))  ; :include-macros for CLJS

;; Define a generic with type hierarchy
(defg combine [a b]
  :vec    (into a b)
  :map    (merge a b)
  :set    (into a b)
  :seq    (concat a b)
  :string (str a b)
  (throw (ex-info "Cannot combine" {:a a :b b})))

(combine [1 2] [3 4])       ;=> [1 2 3 4]
(combine {:a 1} {:b 2})     ;=> {:a 1 :b 2}
(combine "hello " "world")  ;=> "hello world"

;; Extend for new types
(generic+ combine [a b]
  :number (+ a b))

(combine 10 20)  ;=> 30

;; Fork a library generic — customize without affecting the original
(fork my-combine combine)
(generic+ my-combine [a b]
  :number (* a b))    ; multiply instead of add

(my-combine 3 4)  ;=> 12
(combine 3 4)     ;=> 30  — original unchanged
```

## How It Works

1. `defg` generates one protocol per arity + a `defn` wrapper + `extend` calls
2. Type keywords (`:vec`, `:map`, etc.) resolve to host classes at **compile time**
3. Aggregate types expand recursively: `:coll` → all leaf classes
4. Dispatch is protocol-based — zero runtime overhead on CLJ
5. Guards add a two-phase pre-check before protocol dispatch

## Documentation

📖 **[Full API Reference →](doc/API.md)** — Complete docs for every macro, extension modes, predicate guards, ClojureScript setup, custom types, type hierarchy, and more.

## Development

```bash
clj -M -e "(require 'thetis.tries.one 'thetis.tries.two 'thetis.tries.three 'thetis.tries.four 'thetis.tries.five)"  # CLJ tests
npx shadow-cljs compile main   # CLJS tests
```

## Status

**v0.2.0** — Macro layer complete and tested (CLJ + CLJS). All extension modes, predicate guards, and fork semantics are stable. Available as git dependency via `io.github.pbaille/thetis`.

## License

[MIT](LICENSE)
