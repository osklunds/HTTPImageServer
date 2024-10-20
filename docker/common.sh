#!/bin/bash

die () {
    echo "$1"
    exit 1
}

check_paths () {
    if [[ ! -d "/thumbnail" ]]; then
        die "/thumbnail does not exist"

    elif [[ ! -d "/full" ]]; then
        die "/full does not exist"
    fi
}
