DBHOST:=192.168.1.136
DBUSER:=bnl
DBNAME:=bnl

.PHONY : all

all  : \
       greenup_lay_first_3d.png \
       greenup_lay_first.png



greenup_lay_first.png : heatmap.gpl greenup_lay_first.dat
	gnuplot -e "type='Win'" \
               -e "title='Horses GB/IE - for for WIN-market, backsize=100 skr back if progressed'" \
               -e "target_png='greenup_lay_first.png'" \
               -e "source_dat='greenup_lay_first.dat'" heatmap.gpl

greenup_lay_first_3d.png : heatmap_3d.gpl greenup_lay_first.dat
	gnuplot -e "type='Win'" \
                -e "title='Horses GB/IE - for WIN-market, laysize=100 skr back if progressed'" \
                -e "target_png='greenup_lay_first_3d.png'" \
                -e "source_dat='greenup_lay_first.dat'" heatmap_3d.gpl


greenup_lay_first.dat : greenup_lay_first_data.dat filter_data.bash
	cat greenup_lay_first_data.dat | ./filter_data.bash > greenup_lay_first.dat


#excel.dat : plot_data.dat filter_data_for_excel.bash
#	cat plot_data.dat | ./filter_data_for_excel.bash > excel.dat

#heatmap.dat :
#	psql --host=$(DBHOST) \
#    --username=$(DBUSER) \
#    --tuples-only \
#    --command="select betname, sum(profit) from abets where status = 'M' and side in ('BACK','LAY') and BETNAME like '%_BACK_AFTER%' group by betname order by betname" \
#    --output=heatmap.dat $(DBNAME)

greenup_lay_first_data.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select reference::float,powerdays, sum(profit) from abets where status = 'M' and BETNAME like 'OK_F2%' group by reference::float,powerdays order by reference::float,powerdays" \
    --output=greenup_lay_first_data.dat $(DBNAME)

.PHONY : clean
clean :
	rm -rf  *.dat

.PHONY : very_clean
very_clean :
	rm -rf  *.dat *.png
