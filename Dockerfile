FROM fpco/alpine-haskell-stack:8.6.5

COPY . /app

RUN cd /app

RUN env STACK_YAML=/app/stack.yaml stack --system-ghc install --local-bin-path /app --flag tldr:static
