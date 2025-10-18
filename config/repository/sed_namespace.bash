#!/bin/bash

XML_FILES=$(ls *.xml)

for f in $XML_FILES ; do
#  echo $f
#  f_xml=${f%.*}
#  echo $f_xml
#  mv $f $f_xml
  mv $f $f.tmp
#  sed "s|<MaAstro xmlns=\"http://www.consafelogistics.com/sattmate\" xmlns:xcl=\"http://www.consafelogistics.com/sattmate\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">|<BnlBot xmlns=\"http://www.nonodev.com/bnlbot\" xmlns:xcl=\"http://www.nonodev.com/bnlbot\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">|g" > $f < $f.tmp
  sed "s|<MaAstro>|<BnlBot xmlns=\"http://www.nonodev.com/bnlbot\" xmlns:xcl=\"http://www.nonodev.com/bnlbot\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">|g" > $f < $f.tmp
#  sed "s|</MaAstro>|</BnlBot>|g" > $f < $f.tmp
  rm $f.tmp
done





