# Browser check (optional)

A headless-browser check of the blog's live pages, using
[puppeteer-core](https://pptr.dev/). It is not part of `rebar3 ct` and needs:

- Node.js 22.12 or newer
- a Chrome or Chromium binary (puppeteer-core does not download one)
- the blog running against a development database

It registers a new user, publishes a post and comments on it, so never point
it at production.

```sh
rebar3 nova serve            # in examples/blog, with the dev database up
cd test/browser
npm ci
CHROME=/usr/bin/google-chrome BASE=http://localhost:8080 node check.mjs
```

Steps:

1. the counter at `/live/counter` counts up and down
2. register and log in through `/register`
3. write and publish a post in the live editor
4. a comment posted by the author appears on an anonymous reader's page

Any console error, page error or HTTP 4xx/5xx response fails the run.
