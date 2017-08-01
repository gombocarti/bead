#!/bin/sh

# Convenience script for building Bead

BEAD_SRC="/development/bead"
STACK_WORK="${BEAD_SRC}/.stack-work"

if [ -d $STACK_WORK -o -L $STACK_WORK ]; then
    echo "ok"
#    stack --stack-yaml "${BEAD_SRC}/stack.yaml" build
else
    echo "Directory or link ${STACK_WORK} is not found in ${BEAD_SRC}."
    exit 1
fi
