#!/usr/bin/env bash

if [[ "$@" =~ "--help" || "$#" == 0 ]]; then
    echo "usage: $0 ENV_FILE COMMAND ARG ..."
    echo "environment file should just be lines like VAR=VAL in shell syntax,"
    echo "suitable for bash sourcing"
    exit 1
fi

envFile="$1"
shift
set -a # automatically export all variables
source "$envFile"
set +a

exec "$@"
