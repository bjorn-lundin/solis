DBHOST:=localhost
DBUSER:=bnl
DBNAME:=bnl

.PHONY : all

all  : \
       win_back_heatmap.png win_back_heatmap_3d.png \
       place_back_heatmap.png place_back_heatmap_3d.png \
       place_lay_back_back_heatmap.png  \
       place_lay_back_back_heatmap_3d.png  \
       place_lay_lay_back_heatmap.png  \
       place_lay_lay_back_heatmap_3d.png  \
       win_lay_back_back_heatmap.png  \
       win_lay_back_back_heatmap_3d.png \
       win_lay_lay_back_heatmap.png  \
       win_lay_lay_back_heatmap_3d.png



win_back_heatmap.png : heatmap.gpl win_back_plot_data.dat
	gnuplot -e "type='Win'" \
               -e "title='Horses GB/IE - for for WIN-market, backsize=100 skr back if progressed'" \
               -e "target_png='win_back_heatmap.png'" \
               -e "source_dat='win_back_plot_data.dat'" heatmap.gpl

win_back_heatmap_3d.png : heatmap_3d.gpl win_back_plot_data.dat
	gnuplot -e "type='Win'" \
                -e "title='Horses GB/IE - for WIN-market, backsize=100 skr back if progressed'" \
                -e "target_png='win_back_heatmap_3d.png'" \
                -e "source_dat='win_back_plot_data.dat'" heatmap_3d.gpl

place_back_heatmap.png : heatmap.gpl place_back_plot_data.dat
	gnuplot -e "type='Place'" \
                -e "title='Horses GB/IE - for PLACE-market, backsize=100 skr if progressed'" \
                -e "target_png='place_back_heatmap.png'" \
                -e "source_dat='place_back_plot_data.dat'" heatmap.gpl

place_back_heatmap_3d.png : heatmap_3d.gpl place_back_plot_data.dat
	gnuplot -e "type='Place'" \
                -e "title='Horses GB/IE - for WIN-market, backsize=100 skr if progressed'" \
                -e "target_png='place_back_heatmap_3d.png'" \
                -e "source_dat='place_back_plot_data.dat'" heatmap_3d.gpl


place_lay_back_back_heatmap.png : heatmap.gpl place_lay_back_back_plot_data.dat
	gnuplot -e "type='Place lay back back'" \
                -e "title='Horses GB/IE backsize=100 skr if progressed'" \
                -e "target_png='place_lay_back_back_heatmap.png'" \
                -e "source_dat='place_lay_back_back_plot_data.dat'" heatmap.gpl

place_lay_back_back_heatmap_3d.png : heatmap_3d.gpl place_lay_back_back_plot_data.dat
	gnuplot -e "type='Place lay back back'" \
                -e "title='Horses  backsize=100 skr if progressed'" \
                -e "target_png='place_lay_back_back_heatmap_3d.png'" \
                -e "source_dat='place_lay_back_back_plot_data.dat'" heatmap_3d.gpl

place_lay_lay_back_heatmap.png : heatmap.gpl place_lay_lay_back_plot_data.dat
	gnuplot -e "type='Place lay lay back'" \
                -e "title='Horses backsize=100 skr if progressed'" \
                -e "target_png='place_lay_lay_back_heatmap.png'" \
                -e "source_dat='place_lay_lay_back_plot_data.dat'" heatmap.gpl

place_lay_lay_back_heatmap_3d.png : heatmap_3d.gpl place_lay_lay_back_plot_data.dat
	gnuplot -e "type='Place lay lay back'" \
                -e "title='Horses  backsize=100 skr if progressed'" \
                -e "target_png='place_lay_lay_back_heatmap_3d.png'" \
                -e "source_dat='place_lay_lay_back_plot_data.dat'" heatmap_3d.gpl

win_lay_back_back_heatmap.png : heatmap.gpl win_lay_back_back_plot_data.dat
	gnuplot -e "type='Win lay back back'" \
                -e "title='Horses backsize=100 skr if progressed'" \
                -e "target_png='win_lay_back_back_heatmap.png'" \
                -e "source_dat='win_lay_back_back_plot_data.dat'" heatmap.gpl

win_lay_back_back_heatmap_3d.png : heatmap_3d.gpl win_lay_back_back_plot_data.dat
	gnuplot -e "type='Win lay back back'" \
                -e "title='Horses backsize=100 skr if progressed'" \
                -e "target_png='win_lay_back_back_heatmap_3d.png'" \
                -e "source_dat='win_lay_back_back_plot_data.dat'" heatmap_3d.gpl

win_lay_lay_back_heatmap.png : heatmap.gpl win_lay_lay_back_plot_data.dat
	gnuplot -e "type='Win lay lay back'" \
                -e "title='Horses backsize=100 skr if progressed'" \
                -e "target_png='win_lay_lay_back_heatmap.png'" \
                -e "source_dat='win_lay_lay_back_plot_data.dat'" heatmap.gpl

win_lay_lay_back_heatmap_3d.png : heatmap_3d.gpl win_lay_lay_back_plot_data.dat
	gnuplot -e "type='Win lay lay back'" \
                -e "title='Horses backsize=100 skr if progressed'" \
                -e "target_png='win_lay_lay_back_heatmap_3d.png'" \
                -e "source_dat='win_lay_lay_back_plot_data.dat'" heatmap_3d.gpl


#plot_data.dat : heatmap.dat filter_data.tcl
#	cat heatmap.dat | ./filter_data.tcl > plot_data.dat

win_back_plot_data.dat : win_back_heatmap.dat filter_data.tcl
	cat win_back_heatmap.dat | ./filter_data.tcl > win_back_plot_data.dat

#lay_plot_data.dat : lay_heatmap.dat filter_data.tcl
#	cat lay_heatmap.dat | ./filter_data.tcl > lay_plot_data.dat

place_back_plot_data.dat : place_back_heatmap.dat filter_data.tcl
	cat place_back_heatmap.dat | ./filter_data.tcl > place_back_plot_data.dat


place_lay_lay_back_plot_data.dat : place_lay_lay_back_heatmap.dat filter_data.tcl
	cat place_lay_lay_back_heatmap.dat | ./filter_data.tcl > place_lay_lay_back_plot_data.dat

place_lay_back_back_plot_data.dat : place_lay_back_back_heatmap.dat filter_data.tcl
	cat place_lay_back_back_heatmap.dat | ./filter_data.tcl > place_lay_back_back_plot_data.dat

win_lay_lay_back_plot_data.dat : win_lay_lay_back_heatmap.dat filter_data.tcl
	cat win_lay_lay_back_heatmap.dat | ./filter_data.tcl > win_lay_lay_back_plot_data.dat

win_lay_back_back_plot_data.dat : win_lay_back_back_heatmap.dat filter_data.tcl
	cat win_lay_back_back_heatmap.dat | ./filter_data.tcl > win_lay_back_back_plot_data.dat





#excel.dat : plot_data.dat filter_data_for_excel.bash
#	cat plot_data.dat | ./filter_data_for_excel.bash > excel.dat

#heatmap.dat :
#	psql --host=$(DBHOST) \
#    --username=$(DBUSER) \
#    --tuples-only \
#    --command="select betname, sum(profit) from abets where status = 'M' and side in ('BACK','LAY') and BETNAME like '%_BACK_AFTER%' group by betname order by betname" \
#    --output=heatmap.dat $(DBNAME)

win_back_heatmap.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where status = 'M' and BETNAME like 'WIN_BACK_AFTER_%' group by betname order by betname" \
    --output=win_back_heatmap.dat $(DBNAME)

#lay_heatmap.dat :
#	psql --host=$(DBHOST) \
#    --username=$(DBUSER) \
#    --tuples-only \
#    --command="select betname, sum(profit) from abets where status = 'M' and side in ('LAY') and BETNAME like 'WINGREENUP_LAY_FIXED_LOSS_TICS%' group by betname order by betname" \
#    --output=lay_heatmap.dat $(DBNAME)

place_back_heatmap.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where status = 'M' and BETNAME like 'PLC_BACK_AFTER_%' group by betname order by betname" \
    --output=place_back_heatmap.dat $(DBNAME)

place_lay_lay_back_heatmap.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where side = 'LAY' and BETNAME like 'PLC_LAYBACK_AFTER_%' group by betname order by betname" \
    --output=place_lay_lay_back_heatmap.dat $(DBNAME)

place_lay_back_back_heatmap.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where side = 'BACK' and BETNAME like 'PLC_LAYBACK_AFTER_%' group by betname order by betname" \
    --output=place_lay_back_back_heatmap.dat $(DBNAME)

win_lay_lay_back_heatmap.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where side = 'LAY' and BETNAME like 'WIN_LAYBACK_AFTER_%' group by betname order by betname" \
    --output=win_lay_lay_back_heatmap.dat $(DBNAME)

win_lay_back_back_heatmap.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where side = 'BACK' and BETNAME like 'WIN_LAYBACK_AFTER_%' group by betname order by betname" \
    --output=win_lay_back_back_heatmap.dat $(DBNAME)


.PHONY : clean
clean :
	rm -rf  *.dat

.PHONY : very_clean
very_clean :
	rm -rf  *.dat *.png
