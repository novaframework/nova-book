# Introduction

## What is Nova?

[Nova](https://github.com/novaframework/nova) is a web framework for Erlang/OTP. It handles routing, request processing, template rendering, sessions, and WebSockets: the core pieces you need to build web applications and APIs. Nova sits on top of [Cowboy](https://github.com/ninenines/cowboy), the battle-tested Erlang HTTP server, and adds a structured layer for organizing your application.

## What this book covers

Nova is the centre of this book, but not all of it. A web application needs more than a framework, and in Erlang the rest comes from separate libraries that work with Nova: Kura for the database, nova_auth for authentication, Arizona for live views, Hikyaku for email, Shigoto for background jobs and opentelemetry_nova for observability. The book teaches each of them where the application needs it, so by the end you can build, test and run a complete web application in Erlang, not just its Nova part.

## Who this book is for

This book is for anyone who wants to build web applications with Erlang, whether you are an experienced developer exploring a new stack or a newcomer picking up Erlang for the first time. If you have built anything with another web framework (Express, Rails, Django, Phoenix, etc.) you will feel right at home, but it is not a requirement. Basic familiarity with HTTP and databases is enough to get started.

No prior Erlang experience is needed. The [Erlang Essentials](appendix/erlang-essentials.md) appendix covers the language fundamentals you will use throughout the book, and [Learn You Some Erlang](https://learnyousomeerlang.com/) is an excellent free companion if you want a deeper introduction. You can start the book right away and refer back to these resources as you go.

## What you'll build

Throughout this book you will build a **blog platform** step by step:

1. **A Nova application from scratch** - project structure, routing, controllers and plugins
2. **A data layer with Kura** - schemas, migrations, changesets, associations and queries on PostgreSQL
3. **Authentication & sessions** - login flows, security functions, authorization
4. **An HTML frontend** - ErlyDTL templates, layouts, forms with validation
5. **A JSON API** - RESTful endpoints, OpenAPI documentation, error handling
6. **Real-time features with Arizona** - live views, stateful components, differential rendering
7. **Real-time infrastructure** - WebSockets, pub/sub and a live comment section
8. **Email with Hikyaku** - registration confirmation, password reset, notifications
9. **Background jobs with Shigoto** - durable, retried jobs stored in PostgreSQL
10. **Testing** - EUnit, Common Test and `nova_test`
11. **Production** - configuration, OpenTelemetry observability, custom plugins, security, deployment

The blog has users who write posts, readers who leave comments, and tags for organizing content. That exercises the whole ecosystem: Nova for the web layer, Kura (with the minato PostgreSQL driver) for data, Arizona for real-time views, Hikyaku for email and Shigoto for work that should not happen inside a request.

The finished application is in [`examples/blog`](https://github.com/novaframework/nova-book/tree/main/examples/blog) in the book's repository. Every code sample in the book comes from it, except those marked as variants or alternatives, and its test suite exercises what each chapter describes.

```admonish info title="Prerequisites"
Before starting, make sure you have:

- **Erlang/OTP 29** - install it with [mise](https://mise.jdx.dev/) (recommended) or your system package manager. Nova, Kura and Shigoto run on OTP 28, but Arizona (Part VI) needs OTP 29, and the companion app is tested on OTP 29.
- **rebar3** 3.25 or newer - also installable with mise
- **Docker** - for running PostgreSQL with Docker Compose
- A text editor and a terminal

The book uses Nova 0.18, Kura 2.25, Shigoto 1.10, Hikyaku 0.2 and Arizona 0.4. See the [Erlang Essentials](appendix/erlang-essentials.md) appendix for setup instructions.
```

## How to read this book

The chapters are designed to be read in order. Each one builds on the previous: the application grows progressively from a bare project to a full-featured, deployed service. Code examples accumulate, so what you build in Part I is extended in Part II and brought to life in Part VI.

If you are already familiar with Nova, you can jump to specific parts. The [Cheat Sheet](appendix/cheat-sheet.md) appendix is a useful standalone reference.

Let's get started by [creating your first Nova application](foundations/create-app.md).
