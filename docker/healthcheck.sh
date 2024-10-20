#!/bin/bash

source ./common.sh

check_paths

curl -f http://127.0.0.1:80 || exit 1
