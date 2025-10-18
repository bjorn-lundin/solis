#!/bin/bash

XML_FILES=$(ls *.xml)

for f in $XML_FILES ; do
#  echo $f
#  f_xml=${f%.*}
#  echo $f_xml
#  mv $f $f_xml
  mv $f $f.tmp
  sed "s|type=\"foreign\"|type=\"index\"|g" > $f < $f.tmp
  rm $f.tmp
  mv $f $f.tmp
  sed "s|type=\"candidate\"|type=\"unique\"|g" > $f < $f.tmp
  rm $f.tmp
done





