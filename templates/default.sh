#!/usr/bin/env bash

set -eu

if [[ -z ${DEBUG+x} ]]; then
    debug=echo
else
    debug=:
fi

