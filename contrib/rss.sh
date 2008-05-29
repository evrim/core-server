#!/bin/sh
ROOTPATH=$CORESERVER_HOME
darcs changes --xml --from-match 'date "7 days ago"' --repo=$ROOTPATH/lib/core-server | xsltproc $ROOTPATH/lib/core-server/contrib/rss.xslt -
