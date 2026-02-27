# Introduction

## What is Nova?

[Nova](https://github.com/novaframework/nova) is a web framework for Erlang/OTP. It handles routing, request processing, template rendering, sessions, and WebSockets — the core pieces you need to build web applications and APIs. Nova sits on top of [Cowboy](https://github.com/ninenines/cowboy), the battle-tested Erlang HTTP server, and adds a structured layer for organizing your application.

## Who this book is for

This book is for anyone who wants to build web applications with Erlang — whether you are an experienced developer exploring a new stack or a newcomer picking up Erlang for the first time. If you have built anything with another web framework (Express, Rails, Django, Phoenix, etc.) you will feel right at home, but it is not a requirement. Basic familiarity with HTTP and databases is enough to get started.

No prior Erlang experience is needed. The [Erlang Essentials](appendix/erlang-essentials.md) appendix covers the language fundamentals you will use throughout the book, and [Learn You Some Erlang](https://learnyousomeerlang.com/) is an excellent free companion if you want a deeper introduction. You can start the book right away and refer back to these resources as you go.

## What you'll build

Throughout this book you will build a **blog platform** step by step:

1. **A Nova application from scratch** — project structure, routing, controllers, and plugins
2. **A database layer with Kura** — schemas, migrations, changesets, associations, and advanced queries
3. **Authentication & sessions** — login flows, security callbacks, role-based authorization
4. **An HTML frontend** — ErlyDTL templates, layouts, forms with validation
5. **A JSON API** — RESTful endpoints with code generators, OpenAPI documentation, error handling
6. **Real-time features with Arizona** — live views, stateful components, differential rendering
7. **Real-time infrastructure** — WebSockets, pub/sub, and a live comment section
8. **Email delivery with Hikyaku** — registration confirmation, password reset, notifications
9. **Testing** — unit tests with EUnit, integration tests with Common Test, real-time testing
10. **Production** — configuration, OpenTelemetry observability, custom plugins, deployment

The blog has users who write posts, readers who leave comments, and tags for organizing content. This naturally exercises the full Nova ecosystem: Kura for the database layer, Arizona for real-time interactivity, Hikyaku for transactional email, and Nova's plugin system for cross-cutting concerns.

```admonish info title="Prerequisites"
Before starting, make sure you have:

- **Erlang/OTP 28+** — install via [mise](https://mise.jdx.dev/) (recommended), [asdf](https://asdf-vm.com/), or your system package manager. OTP 28 is required for Arizona.
- **Rebar3** — the Erlang build tool, also installable via mise/asdf
- **Docker** — for running PostgreSQL (we use Docker Compose throughout)
- A text editor and a terminal

See the [Erlang Essentials](appendix/erlang-essentials.md) appendix for detailed setup instructions.
```

## How to read this book

The chapters are designed to be read in order. Each one builds on the previous — the application grows progressively from a bare project to a full-featured, deployed service. Code examples accumulate, so what you build in Part I is extended in Part II and brought to life in Part VI.

If you are already familiar with Nova, you can jump to specific parts. The [Cheat Sheet](appendix/cheat-sheet.md) appendix is a useful standalone reference.

Let's get started by [creating your first Nova application](foundations/create-app.md).
