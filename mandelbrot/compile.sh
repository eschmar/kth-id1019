#!/bin/bash

# Compile script for Mac OSX
rm -f complex_nif.so;
gcc -std=c99 -fPIC -shared -o complex_nif.so complex_nif.c -I /usr/local/lib/erlang/erts-11.1.5/include/ -flat_namespace -undefined suppress;