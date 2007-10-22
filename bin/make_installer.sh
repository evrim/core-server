#!/bin/sh
# [ Core-serveR ] installer 2
# Episode: Generate from repo
TAR=`which tar`
MKTEMP=`which mktemp`
MKDIR=`which mkdir`
CP=`which cp`
DIR=`$MKTEMP -d`
TARBALL="core-server-installer-`date +\"%Y-%m-%d\"`.tar.gz"

CORESERVER_HOME=`cat src/install/install.sh 2> /dev/null`
if [ -z $CORESERVER_HOME ]; then
    CORESERVER_HOME=`cat ../src/install/install.sh 2> /dev/null`;
    if [ -z $CORESERVER_HOME ]; then
	echo "Where am i?";
	exit 1;
    else
	CORESERVER_HOME="$(pwd)/../";
    fi    
else
   CORESERVER_HOME="$(pwd)/";
fi
echo $CORESERVER_HOME
$MKDIR -p $DIR/core-server-installer;
cd $DIR;
$CP $CORESERVER_HOME/src/install/* core-server-installer;
$CP $CORESERVER_HOME/doc/README core-server-installer;
$TAR zcf $TARBALL *
mv $TARBALL /tmp/
echo "[Core serveR] Installer tarball is ready: /tmp/$TARBALL"
