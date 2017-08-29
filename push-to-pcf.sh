#!/bin/sh
APPNAME=servant-health
BIN_ROOT=$($HOME/.local/bin/stack path --local-install-root)
BINARY_PATH="$BIN_ROOT/bin/servant-pcf"
BUILDPACK_URL="https://github.com/cloudfoundry/binary-buildpack.git"
MEMORY=100MB
CMD="cf push $APPNAME -c $BINARY_PATH -b $BUILDPACK_URL -m $MEMORY"
echo "Running command:"
echo $CMD
$CMD


