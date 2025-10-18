#!/bin/bash

f1_old="x"

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
    # GREENUP_LAY_FIXED_LOSS_TICS_05_85.00 | -16948.00



    f1=$(echo $line | cut -d'|' -f1)
    f2=$(echo $line | cut -d'|' -f2)
    f3=$(echo $line | cut -d'|' -f3)
    #    38 |        24 |  -2553.10
    #f1=38
    #f2=24
    #f3=-2553.10

    if [ "$f1" != "$f1_old" ] ; then
      echo ""
      f1_old=$f1
    fi

    echo "$f1|$f2|$f3"
  fi

done < "${1:-/dev/stdin}"




