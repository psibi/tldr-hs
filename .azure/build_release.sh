#!/usr/bin/env bash

set -eux

export PATH=$HOME/.local/bin:$PATH

# Reg: BUILD_BINARIESDIRECTORY
# https://docs.microsoft.com/en-us/azure/devops/pipelines/tasks/utility/archive-files?view=azure-devops

rm -rf "${BUILD_BINARIESDIRECTORY}"
mkdir "${BUILD_BINARIESDIRECTORY}"

stack install --local-bin-path "${BUILD_BINARIESDIRECTORY}" --flag tldr:static
