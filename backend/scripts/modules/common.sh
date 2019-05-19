#!/bin/sh

SCRIPT_PATH=$(realpath $0)
SCRIPT_PREFIX=$(dirname ${SCRIPT_PATH})
BEAD_CONF=${SCRIPT_PREFIX}/../../etc/bead.conf

PENDING=".pending"
LOCKED=".locked"

msg_n() {
    local tz

    tz="UTC"
    date=$(env TZ=$tz date '+%Y.%m.%d. %H:%M:%S')
    echo -n "[${date}] $1";
}

msg() {
    msg_n "$1";
    echo ""
}

msg_verbose() {
    [ ${VERBOSE:-0} -gt 0 ] || return 0
    msg "$1"
}

msg_debug() {
    [ ${VERBOSE:-0} -gt 1 ] || return 0
    msg "DEBUG: $1" >&2
}

check_encoding() {
    local f
    local g
    local r

    f=$1

    [ ! -s $f ] && return 0

    g=$(mktemp)
    (echo "This is a workaround for file(1), you should not see it."; cat $f) > $g
    case $(file -b $g) in
      *extended-ASCII\ text*) r=1;;
      *ASCII\ text*) r=0;;
      *UTF-8\ Unicode\ text*) r=0;;
      *) r=1;;
    esac
    rm -f $g
    return $r
}

correct_encoding() {
    local f
    f=$1
    tmp=$(mktemp)

    cat > $tmp <<EOF
Unfortunately, the contents of this comment contains some non-Unicode
(UTF-8) characters. We tried to do our best to convert it into a
proper UTF-8 text and it may not display correctly as a result. 

Please use UTF-8 in all of your source files and output of your program.

EOF
    iconv -c --to-code=UTF-8 $f >> $tmp
    mv -f $tmp $f
}

force_publish() {
    local src
    local tgt

    src=$1
    tgt=$2

    if [ -s ${src} ] && [ -d $(dirname ${tgt}) ]; then
        cp --preserve=timestamps -f ${src} ${tgt}
        chmod g+rw,o+rw ${tgt}
    fi
}

publish() {
    check_encoding $1 || correct_encoding $1
    force_publish $1 $2
}

# Path variables
JAIL_PATH=
JOB_PATH=
OUTPUT_DIR=
OUTPUT_DIR_TMP=
OUTPUT=
RESULT=
SUBMISSION_ID=
SUBMISSION_ID_OUT=
TEMPLATES=
BUILD_PATH=
SANDBOX_PATH=

set_paths() {
  local JAILNAME=$1
  local JOB_ID=$2
  JAIL_PATH="${JAILS_PATH}/${JAILNAME}"
  JOB_PATH="${JAILS_PATH}/${JAILNAME}/job/${JOB_ID}"
  OUTPUT_DIR="${JOB_PATH}/result"
  OUTPUT="${OUTPUT_DIR}/private"
  MESSAGE="${OUTPUT_DIR}/public"
  RESULT="${OUTPUT_DIR}/result"
  SUBMISSION_ID="${JOB_PATH}/id"
  SUBMISSION_ID_OUT="${OUTPUT_DIR}/id"
  TEMPLATES="${SCRIPT_PREFIX}/../templates"
  BUILD_PATH="${JAIL_PATH}/build"
  SANDBOX_PATH="${JAIL_PATH}/run"
}

msg_debug "Checking for ${BEAD_CONF}..."

if [ -f ${BEAD_CONF} ]; then
    . ${BEAD_CONF}
    msg_debug "OK"
else
    msg_debug "NOT FOUND"
fi

# Some reasonable default
: ${WATCHDOG_TIMEOUT:=15}
: ${SLEEP_TIME:=5}
: ${JOBS_PATH:=/home/bead/jobs}
: ${JAILS_PATH:=/home/tester/jails}
: ${BEAD_HOME:=/home/tester}
: ${ULIMIT_BUILD:=16}
: ${ULIMIT_RUN:=16}

msg_debug "Watchdog timeout = ${WATCHDOG_TIMEOUT}"
msg_debug "Sleep time = ${SLEEP_TIME}"
msg_debug "BE-AD home = ${BEAD_HOME}"
