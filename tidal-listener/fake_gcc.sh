#!/bin/bash

# hint barely uses gcc at all, only for queries of the following form:
# 
#  $ gcc -fno-stack-protector -DTABLES_NEXT_TO_CODE -B/root/my-program/haskell-libs/integ_2aU3IZNMF9a7mQ0OzsZ0dS --print-file-name libgmp.so
#  libgmp.so
# 
# We can easily fake it by always returning the last argument.

while [ "$2" ]; do shift; done
echo "$1"