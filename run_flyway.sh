#!/bin/bash

WORKING_DIRECTORY=`dirname $0`
cd ${WORKING_DIRECTORY}
# Let's move to top folder

VERSION="7.5.1"

LINUX_TARBALL_URL="https://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/${VERSION}/flyway-commandline-${VERSION}-linux-x64.tar.gz"

OSX_TARBALL_URL="https://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/${VERSION}/flyway-commandline-${VERSION}-macosx-x64.tar.gz"

INSTALL_LOCATION="./tmp/flyway-${VERSION}/"

mkdir -p ./tmp

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     URL=${LINUX_TARBALL_URL};;
    Darwin*)    URL=${OSX_TARBALL_URL};;
    CYGWIN*)    echo "CYGWIN not supported"; exit 1;;
    MINGW*)     echo "MINGW not supported"; exit 1;;
    *)          echo "Unknown machine not supported"; exit 1
esac


echo "Machine: ${unameOut}"

if [ -d "$INSTALL_LOCATION" ]; then
  echo "Directory: $INSTALL_LOCATION exists"
else
  TAR_DOWNLOAD_LOCATION="./tmp/flyway.tar.gz"

  wget -O $TAR_DOWNLOAD_LOCATION $URL > /dev/null || exit 1
  tar -xzvf $TAR_DOWNLOAD_LOCATION -C ./tmp/ > /dev/null || exit 1
fi


COMMAND="migrate"
if [ -n "$1" ]; then
  COMMAND="$1"
fi

cp flyway.conf ./tmp/flyway-${VERSION}/conf || exit 1

$INSTALL_LOCATION/flyway $COMMAND || exit 1












