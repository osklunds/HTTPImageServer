#!/bin/bash

cd ..
output="$(cabal repl 2>&1 <<EOF
:l Server.Tests
runTests
EOF
 )"

nl=$'\n'
if [[ "$output" =~ "True"$nl"ghci> Leaving GHCi." ]]; then
    echo "Passed tests"
    exit 0
elif [[ "$output" =~ "False"$nl"ghci> Leaving GHCi." ]]; then
    echo "Failed tests"
    exit 1
else
    echo "Couldn't match output"
    exit 1
fi

