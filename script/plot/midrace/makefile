DBHOST:=192.168.1.20
DBUSER:=bnl
DBNAME:=bnl

.PHONY : all 

PNGS:= \
gd3_0.50_1_PLC.png gd3_0.50_1_WIN.png gd3_0.50_2_PLC.png gd3_0.50_2_WIN.png gd3_0.50_3_PLC.png gd3_0.50_3_WIN.png \
gd3_0.55_1_PLC.png gd3_0.55_1_WIN.png gd3_0.55_2_PLC.png gd3_0.55_2_WIN.png gd3_0.55_3_PLC.png gd3_0.55_3_WIN.png \
gd3_0.60_1_PLC.png gd3_0.60_1_WIN.png gd3_0.60_2_PLC.png gd3_0.60_2_WIN.png gd3_0.60_3_PLC.png gd3_0.60_3_WIN.png \
gd3_0.65_1_PLC.png gd3_0.65_1_WIN.png gd3_0.65_2_PLC.png gd3_0.65_2_WIN.png gd3_0.65_3_PLC.png gd3_0.65_3_WIN.png \
gd3_0.70_1_PLC.png gd3_0.70_1_WIN.png gd3_0.70_2_PLC.png gd3_0.70_2_WIN.png gd3_0.70_3_PLC.png gd3_0.70_3_WIN.png \
gd3_0.75_1_PLC.png gd3_0.75_1_WIN.png gd3_0.75_2_PLC.png gd3_0.75_2_WIN.png gd3_0.75_3_PLC.png gd3_0.75_3_WIN.png \
gd3_0.80_1_PLC.png gd3_0.80_1_WIN.png gd3_0.80_2_PLC.png gd3_0.80_2_WIN.png gd3_0.80_3_PLC.png gd3_0.80_3_WIN.png \
gd3_0.85_1_PLC.png gd3_0.85_1_WIN.png gd3_0.85_2_PLC.png gd3_0.85_2_WIN.png gd3_0.85_3_PLC.png gd3_0.85_3_WIN.png \
gd3_0.90_1_PLC.png gd3_0.90_1_WIN.png gd3_0.90_2_PLC.png gd3_0.90_2_WIN.png gd3_0.90_3_PLC.png gd3_0.90_3_WIN.png \
gd3_0.95_1_PLC.png gd3_0.95_1_WIN.png gd3_0.95_2_PLC.png gd3_0.95_2_WIN.png gd3_0.95_3_PLC.png gd3_0.95_3_WIN.png

all: $(PNGS)

#save the intermediate files too
.PRECIOUS : %.dat

# implict rules
# $* is the stem
# $@ is the target file name

   
%.dat: $(BOT_TARGET)/bin/graph_data_3
	$(BOT_TARGET)/bin/graph_data_3 --logfile=$(shell echo $* | cut -d'_' -f2)_$(shell echo $* | cut -d'_' -f3)_$(shell echo $* | cut -d'_' -f4) \
                                       --percent=$(shell echo $* | cut -d'_' -f2) \
                                       --runner=$(shell echo $* | cut -d'_' -f3) \
                                       --markettype=$(shell echo $* | cut -d'_' -f4) > \
              $(BOT_SCRIPT)/plot/midrace/gd3_$(shell echo $* | cut -d'_' -f2)_$(shell echo $* | cut -d'_' -f3)_$(shell echo $* | cut -d'_' -f4).dat
#	@echo "done gd3 $* - $@ - $<"        


%.png:  midrace.gpl %.dat
	gnuplot \
            -e "percent='$(shell echo $* | cut -d'_' -f2)'" \
            -e "runner='$(shell echo $* | cut -d'_' -f3)'" \
            -e "markettype='$(shell echo $* | cut -d'_' -f4)'" \
            -e "datafile='$*.dat'" \
            -e "target_png='$*.png'" midrace.gpl



.PHONY : clean
clean :
	rm -rf  *.png *.dat

