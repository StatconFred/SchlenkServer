#!/bin/bash


cd /home/statcon
curdate="$(date +"%Y_%m_%d_%I_%M")"
mkdir "/home/statcon/backup/${curdate}"
cp /home/statcon/db.r "/home/statcon/backup/${curdate}/"
cp /home/statcon/shiny-server/app.R "/home/statcon/backup/${curdate}/"
cd /home/statcon/backup/
tar -czf "bak_${curdate}.tgz" $curdate
rm -r "/home/statcon/backup/${curdate}/"
cd /home/statcon
