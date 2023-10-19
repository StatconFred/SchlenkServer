#!/bin/bash


#!/bin/bash
DATE=`date`

echo "$DATE  Start Schlenk --------------------------" >>/home/statcon/db_log/sl_cronjob.log
DATE=`date`

/usr/bin/Rscript --no-save --no-restore  /home/statcon/db.r 1>>/home/statcon/db_log/sl_cronjob.log 2>>/home/statcon/db_log/sl_cronjob.log

DATE=`date`
echo "$DATE  Stop Schlenk ---------------------------" >>/home/statcon/db_log/sl_cronjob.log



