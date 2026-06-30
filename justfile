# List all recipes
default:
	just --list --unsorted

# Run
run:
	stack exec -- tldr --update

# Test via ls
ls:
	stack exec -- tldr ls

# Build image
build-image:
	docker build --file Dockerfile . --tag tldr

# Copy static binary from container
copy-static-tldr: build-image
	#!/usr/bin/env bash
	set -euo pipefail
	CID=$(docker create tldr)
	docker cp ${CID}:/app/tldr .
	docker rm ${CID}
	ls -lah tldr

# Sanity test the binary
test:
	file ./tldr
	-mkdir -p ~/.local/share/tldr
	./tldr --update
	./tldr ls
