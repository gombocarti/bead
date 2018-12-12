#!/bin/sh

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})

. ${SCRIPT_PREFIX}/common.sh

JAILNAME=$1
JOB_ID=$2
TIMEOUT=$3

read TESTER_PID

set_paths ${JAILNAME} $JOB_ID

test -z "$OUTPUT_DIR" && exit 1
test -z "$OUTPUT" && exit 1

sleep ${TIMEOUT}

if kill -9 $TESTER_PID; then
    pgrep -U test_runner | ${SCRIPT_PREFIX}/Kill
    msg "[watchdog] Had to kill test_runner's all processes."
    mkdir -p ${OUTPUT_DIR_TMP}
    chmod g+w,o+w ${OUTPUT_DIR_TMP}
    watchdog_output=$(mktemp)
    watchdog_message=$(mktemp)
    watchdog_result=$(mktemp)
    echo "Testing of this solution has exceeded the time limit of ${TIMEOUT} seconds." > ${watchdog_output} 2>&1
    echo "Sorry, but automated testing of the solution was given up due to resource limits." > ${watchdog_message} 2>&1
    echo "False" > ${watchdog_result} 2>&1
    force_publish ${watchdog_output} ${OUTPUT}
    force_publish ${watchdog_message} ${MESSAGE}
    force_publish ${watchdog_result} ${RESULT}
    mv -T ${OUTPUT_DIR_TMP} ${OUTPUT_DIR}
    ${SCRIPT_PREFIX}/Drop "${JOB_ID}"
fi
