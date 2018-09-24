#!/bin/sh

ln -s /development/init/.stack-work /development/bead/
cd /development/bead
stack build --flag Bead:Tests --flag Bead:MySQL --flag Bead:SSO && \
stack exec BeadTest
