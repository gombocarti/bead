#!/bin/bash

usage() {
    echo "bead bulk [jailname]

Parameters:
    jailname     -- Jail to use
"
    exit 1
}

JAILNAME="$1"

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})

. ${SCRIPT_PREFIX}/common.sh

INCOMING_DIR="${JOBS_PATH}/${JAILNAME}/incoming"


#if [ ! -d "${INCOMING_DIR}" ]; then
#    msg "${INCOMING_DIR} cannot be found."
#    exit 1
#fi

JAIL_PATH="${BEAD_HOME}/jails/${JAILNAME}"

if [ ! -d "${JAIL_PATH}" ]; then
    msg "Jail ${JAILNAME} cannot be found."
    exit 1
fi

LOOP=1

main_loop() {
    local job

    while [ "${LOOP}" -ne "0" ]; do
        ${SCRIPT_PREFIX}/Cleanup "${JAIL_PATH}/build" "${JAIL_PATH}/run" "${JAIL_PATH}/job"

        job="$(${SCRIPT_PREFIX}/Take)"
        if [ "${job}" = "" ]; then
            msg_verbose "No job found, sleeping for ${SLEEP_TIME} seconds."
            sleep ${SLEEP_TIME}
        else
            subm_id="$(cat "$JAIL_PATH/job/$job/id")"
            msg "Found job ${job}/${subm_id}, evaluating."
            coproc watchdog (${SCRIPT_PREFIX}/watchdog.sh "${JAILNAME}" "${job}" ${WATCHDOG_TIMEOUT})

            ${SCRIPT_PREFIX}/test.sh "${JAILNAME}" "${job}" $watchdog_PID &
            test_PID=$!

            # Now you have ${WATCHDOG_TIMEOUT} seconds to run (at maximum).
            echo $test_PID >&${watchdog[1]}-

            # Forward messages from watchdog to the log
            echo ""

            DONE=false
            until $DONE; do
                read -u ${watchdog[0]} output || DONE=true
                if [ -n "$output" ]; then
                    echo "$output"
                fi
            done

            wait $test_PID $watchdog_PID
            pgrep -U "test_runner" | ${SCRIPT_PREFIX}/Kill
        fi
    done
}

signal_handler() {
    LOOP=0
}

trap signal_handler INT
trap signal_handler TERM
trap signal_handler KILL
trap signal_handler EXIT

msg "Bulk mode started with jail ${JAILNAME}."
main_loop
msg "Bulk mode stopped with jail ${JAILNAME}."
