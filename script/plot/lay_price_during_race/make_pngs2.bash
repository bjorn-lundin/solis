#!/bin/bash

echo $(pwd)

OLD_PWD=$(pwd)
cd dats2

echo $(pwd)

rm *.png

MARKETS=$(ls 1* | cut -d'_' -f1 | sort | uniq)


for MARKETID in $MARKETS ; do

    echo "treat market ${MARKETID}"
    
    TARGET_PNG=${MARKETID}".png"
    DB_NAME="nono"
    #WINNER="1.114537400_7256679_ww.dat"
    #PLACED="1.114537400_7259549_wp.dat 1.114537400_8091268_wp.dat 1.114537400_7539761_wp.dat"
    #LOSERS="1.114537400_5596467_lo.dat 1.114537400_7267929_lo.dat \
    #1.114537400_7539753_lo.dat 1.114537400_7554251_lo.dat \
    #1.114537400_7222723_re.dat 1.114537400_7269234_lo.dat \
    #1.114537400_7627274_lo.dat 1.114537400_7272303_re.dat \
    #1.114537400_7542180_re.dat 1.114537400_7302352_lo.dat \
    #1.114537400_7546026_re.dat 1.114537400_8255417_lo.dat \
    #1.114537400_7262339_re.dat 1.114537400_7440423_lo.dat \
    #1.114537400_7547998_lo.dat 1.114537400_8298446_lo.dat"
    
    WINNER=$(ls ${MARKETID}_*_ww.dat)
    PLACED=$(ls ${MARKETID}_*_wp.dat)
    LOSERS=$(ls ${MARKETID}_*_lo.dat)

    # bets är för fel market!!!!
    # bet är på PLACE, men här är listat WIN!!!
    BETS_WON=$(ls bets_${MARKETID}_*_wi.dat)
    BETS_LOST=$(ls bets_${MARKETID}_*_lo.dat)
    BETS_NOMATCH=$(ls bets_${MARKETID}_*_nm.dat)
    
    
#    RUNNERS="${WINNER} ${PLACED} ${LOSERS} ${BETS}"
#    -e "runners='${RUNNERS}'" \
   
    echo "Winner : $WINNER"
    echo "Placed : $PLACED"
    echo "Losers : $LOSERS"

    gnuplot \
    -e "db_name='${DBNAME}'" \
    -e "target_png='${TARGET_PNG}'" \
    -e "marketid='${MARKETID}'" \
    -e "bets_won='${BETS_WON}'" \
    -e "bets_lost='${BETS_LOST}'" \
    -e "bets_nomatch='${BETS_NOMATCH}'" \
    -e "winners='${WINNER}'" \
    -e "placed='${PLACED}'" \
    -e "losers='${LOSERS}'"  ../back_prices_during_race2.gpl

done
cd ${OLD_PWD}

