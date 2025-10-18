#!/bin/bash

PLOTFILE=plot.gpl

MARKETID_LIST=$(ls *.dat  | cut -d '_' -f1 | sort | uniq)

for m in $MARKETID_LIST ; do

#  PLOTFILE=$m.gpl
  echo "marketid: $m"
  RUNNER_LIST=$(ls $m*.dat )

  #create the plotfile
  
  
  echo "set datafile commentschars '#'" > $PLOTFILE
  echo "set datafile separator '|'" >> $PLOTFILE
  echo "set terminal pngcairo size 2000,768 background '#ffffff'" >> $PLOTFILE
  echo "set output '$m.png'" >> $PLOTFILE
  echo "set grid" >> $PLOTFILE
  echo "set key top left" >> $PLOTFILE
  echo "set title \"price changes for eventid $m \"" >> $PLOTFILE
  echo "set yrange [0:40]" >> $PLOTFILE
  echo "set xrange [-20000:0]" >> $PLOTFILE
#set y2range [-3000:4000]
#set ytics 500
#set y2tics 500

  echo "set ylabel \"price\"" >> $PLOTFILE

#set bmargin 8

#  how many runners ? 
  let cnt=0
  for r in $RUNNER_LIST ; do
    let cnt=$cnt+1
#    echo "cnt = $cnt"
  done
#  echo " stop cnt = $cnt"
  
  let cur=0
  echo "plot \\" >> $PLOTFILE
  
  
  for r in $RUNNER_LIST ; do
    let cur=$cur+1
#    echo "cur = $cur"
    if (($cur < $cnt)) ; then  
      echo "\"$r\" u 1:4 w lines, \\" >> $PLOTFILE
    else
      echo "\"$r\" u 1:4 w lines" >> $PLOTFILE
    fi    
  done
#  break
  gnuplot $PLOTFILE
done



