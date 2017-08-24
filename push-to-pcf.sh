#!/bin/sh
APPNAME=servant-health
BINARY_PATH=".stack-work/install/x86_64-linux-nopie/lts-9.1/8.0.2/bin/servant-pcf"
BUILDPACK_URL="https://github.com/cloudfoundry/binary-buildpack.git"
MEMORY=100MB
cf push "$APPNAME" -c "$BINARY_PATH" -b "$BUILDPACK_URL" -m "$MEMORY"
