#!/usr/bin/env bash

set -eux
TEST=${1}

case "$TEST" in
  "scalafmt" )
    ./scalafmt --test
    ;;

  "slow" )
    sbt "slow/test"
    ;;

  "unit" )
    sbt ";clean;coverage;unit/test;coverageReport"
    bash <(curl -s https://codecov.io/bash)
    ;;
esac