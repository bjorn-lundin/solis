#!/bin/bash


olddiff="ABC"

while read line
do
  case $line in
    refer*)
      OK=false
    ;;
     ----*)
      OK=false
    ;;
    "")
      OK=false
    ;;
    "     *")
      OK=false
    ;;
         *)
       OK=true
    ;;
  esac
  if [ $OK == "true" ] ; then

    #lay=0002.42,tics=18 |  12.86
    # DIFF_R1_R4_005.00_001.70_PLC_2 | -16948.00



    f1=$(echo $line | cut -d'|' -f1)
    fsum=$(echo $line | cut -d'|' -f2)

    #f1=GREENUP_LAY_FIXED_LOSS_TICS_05_85.00
    #fsum=-16948.00


    diff=$(echo $f1 | cut -d'_' -f4)
    price=$(echo $f1 | cut -d'_' -f5)

#    price_new=$($BOT_TARGET/bin/price_to_tics --value=$price)

    echo "$price|$diff|$fsum"

    if [ $diff != $olddiff ] ; then
      echo ""; # "$diff != $olddiff"
    fi
    olddiff=$diff
  fi

done < "${1:-/dev/stdin}"




