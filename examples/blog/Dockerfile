FROM erlang:29.1.2-slim AS build
RUN apt-get update \
    && apt-get install -y --no-install-recommends git ca-certificates \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /build
COPY rebar.config rebar.lock ./
RUN rebar3 as prod compile --deps_only
COPY config config
COPY priv priv
COPY src src
RUN rebar3 as prod tar \
    && mkdir /release \
    && tar -xzf _build/prod/rel/blog/blog-*.tar.gz -C /release

FROM debian:trixie-slim
RUN apt-get update \
    && apt-get install -y --no-install-recommends libssl3t64 libtinfo6 libstdc++6 libsctp1 ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --home-dir /app --shell /usr/sbin/nologin blog
WORKDIR /app
COPY --from=build --chown=blog:blog /release ./
USER blog
ENV RELX_OUT_FILE_PATH=/tmp
EXPOSE 8080
CMD ["/app/bin/blog", "foreground"]
