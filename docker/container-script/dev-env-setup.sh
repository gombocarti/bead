#!/bin/sh

INIT_DIR="/development/init"
BEAD_SRC="/development/bead"

# Make available stack's working directory
# so Bead can be started from /bead-server
if [ ! -e "${BEAD_SRC}/.stack-work" ]; then
  ln -s "${INIT_DIR}/.stack-work" "${BEAD_SRC}/.stack-work"
fi

# Start MySQL and create database
service mysql start
mysqladmin create bead

