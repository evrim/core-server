#!/bin/sh
PWD=`pwd`
cd $1/lib/rfc2388 && yes | darcs unpull --from-match 'date "Sat Jun 16 15:25:52 EEST 2007"'
cd $1/lib/ucw_dev && yes | darcs unpull --from-match 'date "Thu Jul 12 06:15:54 EEST 2007"'
cd $PWD

