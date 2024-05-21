#!/usr/bin/env sh

exec docker run --rm --interactive --volume /Users/mattias/Development/:/Users/mattias/Development ${ML_LSP_DOCKER_CONTAINER:-"ml/devserver-nodejs:18"} $@
