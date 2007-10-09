#!/bin/sh

# Core-serveR Installation Script - Copyright 2006-2007
# Author: Evrim Ulu <evrim@core.gen.tr>
# www.core.gen.tr

SBCL=`which sbcl 2> /dev/null`
REQS="darcs svn cvs wget tar mv rm ln find chmod chown"
FEATURES=":sb-thread :sb-unicode"
SYSTEM_REQS="apache2ctl sudo useradd apxs2"

function check_sbcl {
    if [ -z $SBCL ]; then
	echo "+----------------------------------------------------------+"
	echo "| SBCL is required for Core-Server installation            |"
	echo "| Please install SBCL>1.0.                                 |"
	echo "+----------------------------------------------------------+"
	echo 
	echo "i.e for gentoo:"
	echo "# USE=\"threads unicode\" emerge sbcl"
	exit 1
    fi
}

function check_requirement {
    if [ -z `which $1 2> /dev/null` ]; then
	echo "+----------------------------------------------------------+"
	echo "| $1 is required for Core-Server installation"
	echo "| Please install $1 and re-run this script. "
	echo "+----------------------------------------------------------+"
	exit 1
    fi
}

function check_feature {
    e="(if (member $1 *features*) (quit :unix-status 0) (quit :unix-status 1))"
    $SBCL --noinform --no-sysinit --no-userinit --eval "$e" 
    if [ $? -ne 0 ]; then
	echo "+----------------------------------------------------------+"
	echo "| $1 feature of SBCL is required for Core-Server. "
	echo "| Please install $1 feature and re-run this script. "
	echo "+----------------------------------------------------------+"
	exit 1
    fi	
}

function banner {
    echo "+-------------------------------------------------------------------------------+"
    echo "|                   Welcome to [ - Core-serveR - ] Project                      |"
    echo "|                           http://www.core.gen.tr                              |"
    echo "+-------------------------------------------------------------------------------+"
    echo
}

function prologue {
    banner;
    echo " This program will aid you to install the server base. Please follow the"
    echo " instructions and report any problems to bilgi@core.gen.tr"
    echo
    sleep 3
}

function usage {
    banner;
    echo " Usage: $0 target-directory"
    exit 1
}

function epilogue {
    echo "+------------------ [ - Core-serveR - ] Installed successfully ---------------+"
    echo "|"
    echo "| Base directory: $1"
    echo "| Init script: $1/bin/core-server"
    echo "| Init lisp script: $1/bin/start.lisp"
    echo "|"
    echo "| To start:  $1/bin/core-server start"
    echo "| To attach: $1/bin/core-server attach"
    echo "+-------------------------------------------------------------------------------+" 
}

check_sbcl;
for i in $REQS; do check_requirement $i; done;
for i in $FEATURES; do check_feature $i; done;

# Do system wide installation
if [ "root" = `whoami` ]; then
    for i in $SYSTEM_REQS; do check_requirement $i; done;
fi

if [ ! 1 -eq $# ]; then usage; fi;

prologue
echo "+ Compiling installation script. Please wait..."
echo 
TARGET="(defparameter tr.gen.core.install::+target-directory+ \"$1\")"
$SBCL --noinform \
  --noprint \
  --no-sysinit \
  --no-userinit \
  --load install.lisp \
  --eval "$TARGET" \
  --load config.lisp \
  --eval "(quit :unix-status 111)"

if [  $? -eq 111 ]; then
    epilogue $1
    exit 0
fi

echo "!! Sorry there is an error occured."
exit 1
