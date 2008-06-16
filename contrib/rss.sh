#!/bin/sh
if [ -z $1 ]; then
  echo "Usage: $0 <coreserver-home>"
  exit 1
fi
ROOTPATH=$1
darcs changes --xml --match 'date "last 7 days"' --repo=$ROOTPATH/lib/core-server | xsltproc $ROOTPATH/lib/core-server/contrib/rss.xslt -
