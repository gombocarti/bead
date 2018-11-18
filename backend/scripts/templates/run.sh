#!/bin/bash

__MESSAGE=

say() {
    __MESSAGE=${__MESSAGE}$1'\n'
}

SANDBOX_PATH="$1"
ULIMIT="$2"
ulimit -t ${ULIMIT}
cd ${SANDBOX_PATH}
. ./script
run
__RUN_RESULT=$?
if [ "${__MESSAGE}" != "" ]; then
    echo -ne "${__MESSAGE}" > ${SANDBOX_PATH}/.message
fi
exit ${__RUN_RESULT}
