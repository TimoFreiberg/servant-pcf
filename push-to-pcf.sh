#!/bin/sh

BIN_PATH=".stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/bin/servant-pcf"
BUILDPACK="https://github.com/cloudfoundry/binary-buildpack.git"
APP_NAME="servant-test"

cf push "$APP_NAME" -c "$BIN_PATH" -b "$BUILDPACK"
