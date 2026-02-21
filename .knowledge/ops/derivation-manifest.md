# Derivation Manifest

Skills read this file to adapt their vocabulary to the nemesis vault.

```yaml
vault_root: ".knowledge"
vocabulary:
  notes: notes
  notes_collection: notes
  note: note
  note_plural: notes
  inbox: inbox
  maps: maps
  map: map
  map_plural: maps
  domain: [clojure-library-design]
  reduce: reduce
  reflect: reflect
  reweave: reweave
  verify: verify
  cmd_reflect: /skill:reflect
  cmd_reweave: /skill:reweave
  cmd_verify: /skill:verify
  topic_map: map
  topic_maps: maps
  topic_map_plural: maps
  extraction_categories: |
    | Category        | Description                                          | Example                                        |
    |-----------------|------------------------------------------------------|-------------------------------------------------|
    | decision        | A design choice with rationale                       | "defg expands to N protocols for arity dispatch" |
    | insight         | A realization about how something works or should work| "Aggregate types need recursive class resolution" |
    | open-thread     | An unresolved design problem                         | "How should forking interact with runtime state?" |
    | question        | Something to investigate                             | "Does shadow-cljs 2.20+ change extend-type?"     |
    | implementation  | A technical detail about how code works              | "The macro emits extend, not extend-type, because..." |
    | observation     | A pattern or friction signal                         | "Case accumulation order keeps causing confusion" |
  subsystems: |
    types       — nemesis.types, type registry, platform abstraction
    generics    — defg, generic+, protocol-per-arity, precedence
    extension   — type+, thing, fork, deft, extend/tune/patch modes
    poly        — WIP refactoring, functional environment model
    cljs-compat — ClojureScript compatibility, shadow-cljs, macro expansion in CLJS
    meta        — vault methodology, cross-cutting concerns
```
