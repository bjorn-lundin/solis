#make png of data-files


cp plot.gpl dats/
cd dats


a=1.001
d=0.001

x=$a
while true; do

  MARKETS=$(ls ${x}*.dat 2>/dev/null | cut -c1-11)

  for MARKETID in $MARKETS ; do
    echo "marketid -> $MARKETID"

    LOSERS=$(ls $MARKETID*loser.dat)
    PLACERS=$(ls $MARKETID*place.dat)
    WINNERS=$(ls $MARKETID*winner.dat)

    gnuplot \
    -e "marketid='$MARKETID'" \
    -e "losers='$LOSERS'" \
    -e "placers='$PLACERS'" \
    -e "winners='$WINNERS'" \
    plot.gpl
    PNGS=$(ls *.png)
    for p in $PNGS ;do
      mv $p ../pngs/
    done
  done

  x=$(echo "$x+$d" | bc)
  echo $x
  if [ $x == "2.000" ] ; then
    break
  fi

done

cd ..

