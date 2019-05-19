#!/bin/bash

usage() {
    echo "bead test [jailname] [job_id]

Parameters:
    jailname     -- Jail to use
    job_id       -- Job ID to test
"
    exit 1
}

JAILNAME="$1"
JOB_ID="$2"
WATCHDOG=$3

test -z "${JAILNAME}" && usage
test -z "${JOB_ID}"   && usage

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})

. ${SCRIPT_PREFIX}/common.sh

_INPUT="${JAILS_PATH}/${JAILNAME}/job/${JOB_ID}"

if [ ! -d "${_INPUT}" ]; then
    msg "Job ${JOB_ID} cannot be found."
    exit 1
fi

JAIL_PATH="${JAILS_PATH}/${JAILNAME}"

if [ ! -d "${JAIL_PATH}" ]; then
    msg "Jail ${JAILNAME} cannot be found."
    exit 1
fi

# INPUT="${_INPUT}${PENDING}"
INPUT=${_INPUT}

# grab job
# rm -rf ${INPUT}
# mv -f ${_INPUT} ${INPUT}

set_paths $JAILNAME $JOB_ID

SUBM_ID="$(cat $SUBMISSION_ID)"

test_build() {
    local build_result
    local result
    local build_log
    local build_msg
    local build_rst

    build_log=/tmp/build.${JOB_ID}.log
    build_msg=${BUILD_PATH}/.message
    build_rst=${BUILD_PATH}/.result

    cp ${INPUT}/submission ${INPUT}/tests ${INPUT}/script ${BUILD_PATH}
    chmod a=rx ${BUILD_PATH}/script
    chmod ug=rw ${BUILD_PATH}/submission ${BUILD_PATH}/tests
    ${SCRIPT_PREFIX}/Test "test_runner" "build.sh" "${BUILD_PATH}" "${SANDBOX_PATH}" "${ULIMIT_BUILD}" > ${build_log} 2>&1
    build_result="$?"
    mkdir -p ${OUTPUT_DIR}
    chmod g+w,o+w ${OUTPUT_DIR}
    if [ "${build_result}" -ne "0" ]; then
        publish ${build_log} ${OUTPUT}
        publish ${build_msg} ${MESSAGE}
        echo "False" > ${build_rst}
        force_publish ${build_rst} ${RESULT}
        force_publish ${SUBMISSION_ID} ${SUBMISSION_ID_OUT}
        result=1
    else
        rm -f ${build_log}
        result=0
    fi
    return ${result}
}

test_run() {
    local run_result
    local run_log
    local run_msg
    local run_rst

    run_log=/tmp/run.${JOB_ID}.log
    run_msg=${SANDBOX_PATH}/.message
    run_rst=${SANDBOX_PATH}/.result

    cp --remove-destination ${INPUT}/script ${SANDBOX_PATH}
    chmod a=rx ${SANDBOX_PATH}/script
    [ -e ${SANDBOX_PATH}/tests ] || { cp ${INPUT}/tests ${SANDBOX_PATH}; chmod ug=rw ${SANDBOX_PATH}/tests; }

    cd ${SANDBOX_PATH}
    ${SCRIPT_PREFIX}/Test "test_runner" "run.sh" "${SANDBOX_PATH}" ${ULIMIT_RUN} > ${run_log} 2>&1
    run_result="$?"

    if [ "${run_result}" -ne "0" ]; then echo "False" > ${run_rst}
    else echo "True" > ${run_rst}; fi

    publish ${run_log} ${OUTPUT}
    publish ${run_msg} ${MESSAGE}
    force_publish ${run_rst} ${RESULT}
    force_publish ${SUBMISSION_ID} ${SUBMISSION_ID_OUT}
    return ${run_result}
}

msg_n "[${JOB_ID}/${SUBM_ID}] Building ($$)..."

test_build
build_result="$?"
echo "result=${build_result}"

if [ "${build_result}" -eq "0" ]; then
    msg_n "[${JOB_ID}/${SUBM_ID}] Running ($$)..."
    test_run
    run_result="$?"
    echo "result=${run_result}"
fi
pkill -P $WATCHDOG
kill -9 $WATCHDOG
${SCRIPT_PREFIX}/Drop "${JOB_ID}"
