#!/bin/bash

show_program() {
    cat "$1" | sed = | sed 'N; s/^/    /; s/ *\(.\{4,\}\)\n/\1  /'
}

say() {
    echo -E "$1" >> "${BUILD_PATH}/.message"
}

BUILD_PATH="$1"
SANDBOX_PATH="$2"
ULIMIT="$3"
ulimit -t ${ULIMIT}
cd ${BUILD_PATH}
. ./script
build
__BUILD_RESULT=$?
if [ ! -s "${BUILD_PATH}/.message" ]; then
    rm -f "${BUILD_PATH}/.message"
fi
exit ${__BUILD_RESULT}
