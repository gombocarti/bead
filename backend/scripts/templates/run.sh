#!/bin/bash

say() {
    echo -E "$1" >> ${SANDBOX_PATH}/.message
}

SANDBOX_PATH="$1"
ULIMIT="$2"
ulimit -t ${ULIMIT}
cd ${SANDBOX_PATH}
. ./script
run
__RUN_RESULT=$?
if [ ! -s "${SANDBOX_PATH}/.message" ]; then
    rm -f "${SANDBOX_PATH}/.message"
fi
exit ${__RUN_RESULT}
