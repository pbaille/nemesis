---
title: ${title}
created: ${date}
updated: ${date}
type: ${type}  # decision | insight | open-thread | question | implementation | observation
subsystem: ${subsystem}  # types | generics | extension | poly | cljs-compat | meta
status: active  # active | resolved | superseded | parked
tags: []
related: []
_schema:
  required: [title, created, type, subsystem, status]
  types:
    title: string
    created: date
    updated: date
    type:
      enum: [decision, insight, open-thread, question, implementation, observation]
    subsystem:
      enum: [types, generics, extension, poly, cljs-compat, meta]
    status:
      enum: [active, resolved, superseded, parked]
    tags:
      type: array
      items: string
      pattern: "^[a-z0-9]+(-[a-z0-9]+)*$"  # hyphenated, no spaces
    related:
      type: array
      items: string
---

${content}
