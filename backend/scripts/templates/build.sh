#!/bin/bash

show_program() {
    cat "$1" | sed = | sed 'N; s/^/    /; s/ *\(.\{4,\}\)\n/\1  /'
}

__MESSAGE=

say() {
    __MESSAGE=${__MESSAGE}$1'\n'
}

BUILD_PATH="$1"
SANDBOX_PATH="$2"
ULIMIT="$3"
ulimit -t ${ULIMIT}
cd ${BUILD_PATH}
. ./script
build
__BUILD_RESULT=$?
if [ "${__MESSAGE}" != "" ]; then
    echo -ne "${__MESSAGE}" > ${BUILD_PATH}/.message
fi
exit ${__BUILD_RESULT}
