#!/bin/bash
exit 0
export PATH=/bin:/usr/bin:$PATH
TZ='Europe/Stockholm'
export TZ


function create_dump () {

  #WD=/data/db_dumps/script
  DATE=$(date +"%d")
  YEAR=$(date +"%Y")
  MONTH=$(date +"%m")

  ROOT_DIR=$1

  TARGET_DIR=${ROOT_DIR}/${YEAR}/${MONTH}/${DATE}

  [ ! -d $TARGET_DIR ] && mkdir -p $TARGET_DIR

#  DB_LIST="bnl dry jmb msm"
  DB_LIST="bnl dry"
  TABLE_LIST="aevents amarkets aprices apriceshistory arunners abets abalances"

  for DBNAME in ${DB_LIST} ; do
    for TABLE in ${TABLE_LIST} ; do
      #pg_dump --schema-only --dbname=${DBNAME} --table=${TABLE} > ${TARGET_DIR}/${DBNAME}_${YEAR}_${MONTH}_${DATE}_${TABLE}_schema.dmp
      #pg_dump --data-only  --dbname=${DBNAME} --table=${TABLE} | gzip > ${TARGET_DIR}/${DBNAME}_${YEAR}_${MONTH}_${DATE}_${TABLE}.tar.gz
      pg_dump --data-only  --dbname=${DBNAME} --table=${TABLE} | gzip > ${TARGET_DIR}/${DBNAME}_${YEAR}_${MONTH}_${DATE}_${TABLE}.zip

      R=$?
      if [ $R -eq 0 ] ; then
        case ${TABLE} in
          abets)      echo "null" > /dev/null ;;
          abalances)  echo "null" > /dev/null ;;
                  *)  psql --no-psqlrc --dbname=${DBNAME} --command="truncate table ${TABLE}" ;;
        esac
      fi
    done
  done

  #DB_LIST="${DB_LIST} ${DBNAME}"
  #
  for DBNAME in ${DB_LIST} ; do
    vacuumdb --dbname=${DBNAME} --analyze
  # # reindexdb --dbname=${DBNAME} --system
  # # reindexdb --dbname=${DBNAME}
  done
}
#exit 0

#make dumps to an existing directory and stop
[ -d /usr2/data ] && create_dump /usr2/data/db_dumps && exit 0
#[ -d /home/bnl/data ] && create_dump /home/bnl/data/db_dumps && exit 0
#[ -d /data ] && create_dump /data/db_dumps && exit 0
#[ -d /bnlbot/bnlbot/botstart/data ] && create_dump /bnlbot/bnlbot/botstart/data/db_dumps && exit 0
