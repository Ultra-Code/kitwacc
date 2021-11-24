#!/bin/bash
 ./test/output
actual="$?"
expected="7"

if [ "$actual" = "$expected" ]; then
echo "output  => $actual"
else
echo "output => $expected expected, but got $actual"
exit 1
fi
