#!/usr/bin/env bash

set -eux

rm -rf "${BUILD_BINARIESDIRECTORY}"
mkdir "${BUILD_BINARIESDIRECTORY}"

stack build --copy-bins "${BUILD_BINARIESDIRECTORY}/"
