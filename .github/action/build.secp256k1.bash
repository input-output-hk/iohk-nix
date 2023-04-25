#!/bin/bash

# I don't understand why this just vanishes.
export PATH=/usr/bin:$PATH

./autogen.sh
./configure $FLAGS --enable-module-schnorrsig --enable-experimental
make
make check

$SUDO make install