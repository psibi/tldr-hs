#!/usr/bin/env bash

set -eux

export PATH=$HOME/.local/bin:$PATH

rm -rf "${BUILD_BINARIESDIRECTORY}"
mkdir "${BUILD_BINARIESDIRECTORY}"

stack install --local-bin-path "${BUILD_BINARIESDIRECTORY}" --flag tldr:static
