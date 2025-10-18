#!/bin/bash

LAY_AT_PRICE="150 160 170 180 190 200 210 220 230 240 250"
MAX_START_PRICE="30 40 50 60 70 80 90 100 110 120 130 140"


for i in $LAY_AT_PRICE ; do
  for j in $MAX_START_PRICE ; do
    $BOT_TARGET/bin/lay_during_race --max_start_price=$j --lay_at_price=$i --max_lay_price=1000  > ${j}_${i}_1000.dat 2>&1 &
  done
done
