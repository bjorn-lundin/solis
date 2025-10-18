#!/bin/bash

files=$(ls *.used)

for f in $files ; do
  b=$(basename $f .used)
  cat $f > ${b}_all.dat
  cat $f | grep "TO BE PLACED" > ${b}_plc.dat
  cat $f | grep -v "TO BE PLACED" > ${b}_win.dat
#  mv $f ${b}.used
done
