#!/bin/bash

rm complex_nif.so;
gcc -std=c99 -fPIC -shared -o complex_nif.so complex_nif.c -I /usr/local/lib/erlang/erts-8.2/include/ -flat_namespace -undefined suppress;