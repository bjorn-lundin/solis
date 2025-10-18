
.PHONY : all


all :
	gnuplot -e "db_name='bnl'" \
      -e "eventid='110776784'" \
      -e "target_png='110776784.png'" \
      -e "data_file1='110776784_6563402.dat'" \
      -e "data_file2='110776784_7085753.dat'" \
      -e "data_file3='110776784_7085753.dat'" \
      -e "data_file4='110776784_7085753.dat'" \
      -e "data_file5='110776784_7085753.dat'" \
      -e "data_file6='110776784_4799922.dat'" price_changes.gpl

.PHONY : datfiles
datfiles:
	$(BOT_TARGET)/bin/plot_race_all_runners --animal=horse --start_date=2013-01-01 --stop_date=2013-12-31