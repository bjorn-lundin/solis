###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

UNAME := $(shell uname)


DBHOST := lundin.duckdns.org

ifeq ($(DBHOST),lundin.duckdns.org)
  DBNAME := flowers
  DBPORT := 5432
  DBPWD  := ld4BC9Q51FU9CYjC21gp
  DBUSER := bnl
endif


#BET_NAMES_REAL := \
# f805fd \
# 96ff06 \
# 9691df \
# f80992


BET_NAMES_REAL := \
temperature pressure humidity gasresistance log co2


BET_NAMES := $(BET_NAMES_REAL)

DATS := $(foreach i,$(BET_NAMES), $(i).dat)
PNGS := $(foreach i,$(BET_NAMES), $(i).png)

all  : png

dat : $(DATS)
png : $(PNGS)


test:
	@echo "---------------------"
	@echo $(BET_NAMES)
	@echo "---------------------"
	@echo $(PNGS)
	@echo "---------------------"
	@echo $(DATS)
	@echo "---------------------"

PHONY : png
#png_all: $(DATS)
#	gnuplot -e "db_name='$(DBNAME)'" \
#            -e "numbets ='0'" \
#            -e "target_png='all.png'" \
#            -e "files='$(BET_NAMES)'" equity.gpl
## equity start ##
#save the intermediate files too
#.PRECIOUS : %.dat

# implict rules
# create a dat and then a png file from that dat
# $* is the stem
# $@ is the target file name


#%.dat:
#	psql --host=$(DBHOST)\
#    --username=$(DBUSER) \
#    --tuples-only \
#    --command="select CREATED, TEMPERATURE from AIRREADINGS where MACADDRESS ='$*' order by CREATED" \
#    --output=tmp.dat $(DBNAME)
#	cat tmp.dat >  $*.dat
#	#cat tmp.dat | awk -F '|' '{i=i+$$2; print $$1,FS,i}' |  head -n-1 >  $*.dat
#	@echo "done gnuplot $* - $@ - $<"


log.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select CREATED, log(GASRESISTANCE) + 0.04* HUMIDITY as result from AIRREADINGS where MACADDRESS ='9691df' order by CREATED" \
    --output=$@ $(DBNAME)


temperature.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select CREATED, TEMPERATURE from AIRREADINGS where MACADDRESS ='9691df' order by CREATED" \
    --output=$@ $(DBNAME)

pressure.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select CREATED, PRESSURE from AIRREADINGS where MACADDRESS ='9691df' order by CREATED" \
    --output=$@ $(DBNAME)


humidity.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select CREATED, HUMIDITY from AIRREADINGS where MACADDRESS ='9691df' order by CREATED" \
    --output=$@ $(DBNAME)


gasresistance.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select CREATED, GASRESISTANCE from AIRREADINGS where MACADDRESS ='9691df' order by CREATED" \
    --output=$@ $(DBNAME)


co2.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select CREATED, PRESSURE from AIRREADINGS where MACADDRESS ='f80992' \
                  and CREATED > '2021-05-14 18:00:00.000' order by CREATED" \
    --output=$@ $(DBNAME)



temperature.png: temperature.dat equity.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='0'" \
            -e "target_png='temperature.png'" \
            -e "files='temperature'" equity.gpl


pressure.png: pressure.dat equity.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='0'" \
            -e "target_png='pressure.png'" \
            -e "files='pressure'" equity.gpl


humidity.png: humidity.dat equity.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='0'" \
            -e "target_png='humidity.png'" \
            -e "files='humidity'" equity.gpl


gasresistance.png: gasresistance.dat equity.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='0'" \
            -e "target_png='gasresistance.png'" \
            -e "files='gasresistance'" equity.gpl


co2.png: co2.dat equity.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='0'" \
            -e "target_png='co2.png'" \
            -e "files='co2'" equity.gpl



log.png: log.dat equity.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='0'" \
            -e "target_png='log.png'" \
            -e "files='log'" equity.gpl




.PHONY : clean
clean :
	rm -rf *.png *.dat
