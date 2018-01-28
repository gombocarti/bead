#!/bin/bash

dev-env-setup && \
  cd /development/bead && \
  stack build --flag Bead:Tests --flag Bead:MySQL --flag Bead:SSO && \
  stack exec BeadTest

