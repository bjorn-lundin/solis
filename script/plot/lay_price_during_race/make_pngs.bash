#!/bin/bash

DAT_DIRECTORY=/home/bnl/bnlbot/botstart/bot-1-0/script/plot/lay_price_during_race/dats
PWD=$(pwd)
cd $DAT_DIRECTORY

MARKETIDS=$(ls | awk -F _ '{ print $1 }' | sort | uniq)
for m in $MARKETIDS ; do
  SELECTIONIDS=$(ls $m* | awk -F _ '{ print $2 }' | sort | uniq)
#  echo $SELECTIONIDS
  gnuplot -e "db_name='nono'" \
            -e "target_png='back_${m}.png'" \
            -e "basename='$m'" \
            -e "selectionids='$SELECTIONIDS'" ../back_prices_during_race.gpl
  gnuplot -e "db_name='nono'" \
           -e "target_png='lay_${m}.png'" \
           -e "basename='$m'" \
           -e "selectionids='$SELECTIONIDS'" ../lay_prices_during_race.gpl
done

cd $PWD