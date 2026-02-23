# Introduction

## What is Nova?

[Nova](https://github.com/novaframework/nova) is a web framework for Erlang/OTP. It handles routing, request processing, template rendering, sessions, and WebSockets — the core pieces you need to build web applications and APIs. Nova sits on top of [Cowboy](https://github.com/ninenines/cowboy), the battle-tested Erlang HTTP server, and adds a structured layer for organizing your application.

## Who this book is for

This book is for experienced web developers who want to build web applications with Erlang. You should be comfortable with at least one other web framework (Express, Rails, Django, Phoenix, etc.) and understand HTTP, REST, and basic database concepts.

You do not need prior Erlang experience. The [Erlang Essentials](appendix/erlang-essentials.md) appendix provides a quick reference and links to learning resources. That said, working through the first few chapters of [Learn You Some Erlang](https://learnyousomeerlang.com/) before starting will make everything smoother.

## What you'll build

Throughout this book you will build a **blog platform** step by step:

1. **A Nova application from scratch** — project structure, routing, and your first controller
2. **An HTML frontend** — login page, views with ErlyDTL templates, authentication and sessions
3. **A database layer with Kura** — schemas, migrations, changesets, and a repository for PostgreSQL
4. **A JSON API** — RESTful endpoints with code generators, associations, preloading, and embedded schemas
5. **Real-time features** — WebSockets and pub/sub for a live comment feed
6. **Production concerns** — transactions, bulk operations, error handling, and deployment
7. **Developer tooling** — OpenAPI documentation, security audits, custom plugins, and OpenTelemetry

The blog has users who write posts, readers who leave comments, and tags for organizing content. This naturally exercises Kura's key features: schemas with associations, enum types (post status), embedded schemas (post metadata as JSONB), changesets with validation, many-to-many relationships (posts and tags), transactions, and bulk operations.

```admonish info title="Prerequisites"
Before starting, make sure you have:

- **Erlang/OTP 27+** — install via [mise](https://mise.jdx.dev/) (recommended), [asdf](https://asdf-vm.com/), or your system package manager
- **Rebar3** — the Erlang build tool, also installable via mise/asdf
- **Docker** — for running PostgreSQL (we use Docker Compose throughout)
- A text editor and a terminal

See the [Erlang Essentials](appendix/erlang-essentials.md) appendix for detailed setup instructions.
```

## How to read this book

The chapters are designed to be read in order. Each one builds on the previous — the application grows progressively from a bare project to a full-featured, deployed service. Code examples accumulate, so what you build in Chapter 2 is extended in Chapter 6 and deployed in Chapter 17.

If you are already familiar with Nova, you can jump to specific chapters. The [Cheat Sheet](appendix/cheat-sheet.md) appendix is a useful standalone reference.

Let's get started by [creating your first Nova application](getting-started/create-app.md).
