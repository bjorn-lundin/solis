#!/bin/bash

tics=$(seq --separator=';' 90)
echo "0;$tics"

tmp=""

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

    #200|01| -4492.50

    f1=$(echo $line | cut -d'|' -f1)
    f2=$(echo $line | cut -d'|' -f2)
    f3=$(echo $line | cut -d'|' -f3)

    tmp="$tmp;$f3"

    if [ $f2 == "90" ] ; then
      #replace '.' with ',' in output via 'tr' 
      echo "$f1$tmp" | tr . ,
      tmp=""
    fi
  fi

done < "${1:-/dev/stdin}"

