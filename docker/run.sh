#!/bin/bash

source ./common.sh

check_paths

exec "HTTPImageServer" "/thumbnail" "/full" 80

