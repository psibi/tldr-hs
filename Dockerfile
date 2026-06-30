FROM docker.io/benz0li/ghc-musl:9.10.3

ADD --chmod=755 https://github.com/commercialhaskell/stack/releases/download/v3.11.1/stack-3.11.1-linux-x86_64-bin /usr/bin/stack

COPY . /app

RUN cd /app

RUN env STACK_YAML=/app/stack.yaml stack --system-ghc install --local-bin-path /app --flag tldr:static
