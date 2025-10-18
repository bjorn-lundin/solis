#make clean && make names && make reps && make dats && make pngs && open *.png

DBHOST:=localhost
DBUSER:=bnl
DBNAME:=bnl

BET_NAMES_1_WIN := $(shell cat names_1_win.dat)
BET_NAMES_2_WIN := $(shell cat names_2_win.dat)
BET_NAMES_1_PLC := $(shell cat names_1_plc.dat)
BET_NAMES_2_PLC := $(shell cat names_2_plc.dat)

#DATS := $(foreach i,$(BET_NAMES), $(i).dat)
#PNGS := $(foreach i,$(BET_NAMES), $(i).png)
#REPS := $(foreach i,$(BET_NAMES), $(i).rep)
REPS_1_WIN := $(foreach i,$(BET_NAMES_1_WIN), $(i).rep)
REPS_2_WIN := $(foreach i,$(BET_NAMES_2_WIN), $(i).rep)
REPS_1_PLC := $(foreach i,$(BET_NAMES_1_PLC), $(i).rep)
REPS_2_PLC := $(foreach i,$(BET_NAMES_2_PLC), $(i).rep)


#.PHONY : all reps dats cfg
#all  : $(PNGS)
#dats  : $(DATS)

.PHONY : all reps
all  : $(REPS)

reps  : $(REPS_1_WIN) $(REPS_2_WIN) $(REPS_1_PLC) $(REPS_2_PLC)

all_names : names_1_win.dat names_2_win.dat names_1_plc.dat names_2_plc.dat
dats : win_1.dat win_2.dat plc_1.dat plc_2.dat
pngs : win_1.png win_2.png plc_1.png plc_2.png win_1_3d.png win_2_3d.png plc_1_3d.png plc_2_3d.png



cfg :
	echo "-----------------------------"
	echo "reps" && echo $(REPS)
	echo "-----------------------------"
	echo "dats" && echo $(DATS)
	echo "-----------------------------"
	echo "pngs" && echo $(PNGS)
	echo "-----------------------------"
	echo "BET_NAMES_1_WIN" && echo $(BET_NAMES_1_WIN)
	echo "-----------------------------"
	echo "BET_NAMES_2_WIN" && echo $(BET_NAMES_2_WIN)
	echo "-----------------------------"
	echo "BET_NAMES_1_PLC" && echo $(BET_NAMES_1_PLC)
	echo "-----------------------------"
	echo "BET_NAMES_2_PLC" && echo $(BET_NAMES_2_PLC)
	echo "-----------------------------"


#create the names
names : names_1_win.dat names_2_win.dat names_1_plc.dat names_2_plc.dat

names_1_win.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select distinct betname from abets where betname like '%WIN%1' order by betname" \
    --output=names_1_win.dat $(DBNAME)

names_2_win.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select distinct betname from abets where betname like '%WIN%2' order by betname" \
    --output=names_2_win.dat $(DBNAME)

names_1_plc.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select distinct betname from abets where betname like '%PLC%1' order by betname" \
    --output=names_1_plc.dat $(DBNAME)

names_2_plc.dat :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select distinct betname from abets where betname like '%PLC%2' order by betname" \
    --output=names_2_plc.dat $(DBNAME)


plc_1.png : heatmap_3d.gpl plc_1.dat
	gnuplot -e "type='PLC backbet - name filter 1'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='1'" \
                -e "target_png='plc_1.png'" \
                -e "source_dat='plc_1.dat'" heatmap_3d.gpl

plc_1_3d.png : heatmap_3d.gpl plc_1.dat
	gnuplot -e "type='PLC backbet - name filter 1'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='0'" \
                -e "target_png='plc_1_3d.png'" \
                -e "source_dat='plc_1.dat'" heatmap_3d.gpl

plc_1.dat : %.rep filter_data.bash
	$(shell cat *PLC*1.rep | ./filter_data.bash > plc_1.dat)

plc_2.png : heatmap_3d.gpl plc_2.dat
	gnuplot -e "type='PLC backbet - name filter 2'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='1'" \
                -e "target_png='plc_2.png'" \
                -e "source_dat='plc_2.dat'" heatmap_3d.gpl

plc_2_3d.png : heatmap_3d.gpl plc_2.dat
	gnuplot -e "type='PLC backbet - name filter 2'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='0'" \
                -e "target_png='plc_2_3d.png'" \
                -e "source_dat='plc_2.dat'" heatmap_3d.gpl

plc_2.dat : %.rep filter_data.bash
	$(shell cat *PLC*2.rep | ./filter_data.bash > plc_2.dat)

win_1.png : heatmap_3d.gpl win_1.dat
	gnuplot -e "type='WIN backbet - name filter 1'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='1'" \
                -e "target_png='win_1.png'" \
                -e "source_dat='win_1.dat'" heatmap_3d.gpl

win_1_3d.png : heatmap_3d.gpl win_1.dat
	gnuplot -e "type='WIN backbet - name filter 1'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='0'" \
                -e "target_png='win_1_3d.png'" \
                -e "source_dat='win_1.dat'" heatmap_3d.gpl

win_1.dat : %.rep filter_data.bash
	$(shell cat *WIN*1.rep | ./filter_data.bash > win_1.dat)

win_2.png : heatmap_3d.gpl win_2.dat
	gnuplot -e "type='WIN backbet - name filter 2'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='1'" \
                -e "target_png='win_2.png'" \
                -e "source_dat='win_2.dat'" heatmap_3d.gpl

win_2_3d.png : heatmap_3d.gpl win_2.dat
	gnuplot -e "type='WIN backbet - name filter 2'" \
                -e "title='Horses backsize=100 skr'" \
                -e "heatmap='0'" \
                -e "target_png='win_2_3d.png'" \
                -e "source_dat='win_2.dat'" heatmap_3d.gpl

win_2.dat : %.rep filter_data.bash
	$(shell cat *WIN*2.rep | ./filter_data.bash > win_2.dat)



%.rep :
	psql --host=$(DBHOST) \
    --username=$(DBUSER) \
    --tuples-only \
    --command="select betname, sum(profit) from abets where status = 'MATCHED' and BETNAME = '$*' group by betname order by betname" \
    --output=$*.rep $(DBNAME)


.PHONY : clean
clean :
	rm -rf  *.dat *.rep *.png
