#!/bin/bash



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
    fsum=$(echo $line | cut -d'|' -f2)

    #f1=GREENUP_LAY_FIXED_LOSS_TICS_05_85.00 
    #fsum=-16948.00


    dtics=$(echo $f1 | cut -d'_' -f6)
    price=$(echo $f1 | cut -d'_' -f7)

    price_new=$($BOT_TARGET/bin/price_to_tics --value=$price)

    echo "$price_new|$dtics|$fsum"

    if [ $price_new == "259" ] ; then
      echo ""
    fi
  fi

done < "${1:-/dev/stdin}"




