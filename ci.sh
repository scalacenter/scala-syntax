#!/usr/bin/env bash

set -eux
TEST=${1}

case "$TEST" in
  "scalafmt" )
    ./scalafmt --test
    ;;

  "slow" )
    ./scalafmt --test
    sbt ";clean;coverage;slow/test;coverageReport"
    bash <(curl -s https://codecov.io/bash)
    ;;

  "unit" )
    sbt "unit/test"
    ;;
esac