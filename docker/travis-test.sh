#!/bin/bash

/development/init/dev-env-setup.sh && \
  cd /development/bead && \
  stack build --flag Bead:Tests --flag Bead:MySQL --flag Bead:SSO && \
  stack exec BeadTest

