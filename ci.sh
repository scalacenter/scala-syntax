#!/usr/bin/env bash

set -eux
TEST=${1}

case "$TEST" in
  "scalafmt" )
    ./scalafmt --test
    ;;

  "slow-idempotent-ast" )
    sbt "slow/testOnly org.scalafmt.internal.IdempotentAstTest"
    ;;

  "slow-preserve-comments" )
    sbt "slow/testOnly org.scalafmt.internal.PreserveCommentsTest"
    ;;
 
  "unit" )
    sbt ";clean;coverage;unit/test;coverageReport"
    bash <(curl -s https://codecov.io/bash)
    ;;
esac