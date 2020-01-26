#!/usr/bin/env bash

set -eux

rm -rf "${BUILD_BINARIESDIRECTORY}"
mkdir "${BUILD_BINARIESDIRECTORY}"

stack install --local-bin-path "${BUILD_BINARIESDIRECTORY}"
