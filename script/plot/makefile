###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

UNAME := $(shell uname)


DBHOST := db.nonodev.com

ifeq ($(DBHOST),db.nonodev.com)
  DBNAME := bnl
  DBPORT := 5432
  DBPWD  := BettingFotboll1$$
  DBUSER := bnl
endif

ifeq ($(DBHOST),sebjlun-deb64)
  DBNAME := bnl
  DBPORT := 5432
  DBPWD  := bnl
  DBUSER := bnl
endif

    
BET_NAMES_DRY := \
  HORSES_WIN_22.0_24.0_6_25_BACK_GB \
  HORSES_WIN_29.0_34.0_6_25_LAY_GB \
  HORSES_WIN_7.4_8.4_6_25_LAY_GB \
  HORSES_WIN_55.0_55.0_6_50_LAY_GB \
  HUMAN_FULL-TIME-SCORE_2.0_9.0_4.0_25_LAY_MX \
  HORSES_WIN_19.5_19.5_6_25_BACK_GB \
  HORSES_WIN_30.0_30.0_6_50_LAY_GB \
  HORSES_WIN_16.5_16.5_6_25_BACK_IE \
  HORSES_WIN_55.0_55.0_6_50_LAY_IE \
  HORSES_WIN_15.5_15.5_6_25_BACK_GB

    
BET_NAMES_REAL := \
  HORSES_PLC_BACK_FINISH_1.15_7.0_1 


BET_NAMES := $(BET_NAMES_REAL)
  
DATS := $(foreach i,$(BET_NAMES), $(i).dat)


# do a wordcount of how many items in array
NUMBETS := $(shell echo $(BET_NAMES) | wc -w )


all  : png

dat : $(DATS)



test:
	@echo "---------------------"
	@echo $(BET_NAMES)
	@echo "---------------------"
	@echo $(PNGS)
	@echo "---------------------"
	@echo $(DATS)
	@echo "---------------------"

PHONY : png
png: $(DATS)
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets ='$(NUMBETS)'" \
            -e "target_png='equity_top_$(NUMBETS).png'" \
            -e "files='$(BET_NAMES)'" equity.gpl
## equity start ##
#save the intermediate files too
#.PRECIOUS : %.dat

# implict rules
# create a dat and then a png file from that dat
# $* is the stem
# $@ is the target file name

   
%.dat:
	psql --host=$(DBHOST)\
    --username=$(DBUSER) \
    --tuples-only \
    --command="select STARTTS,sum(PROFIT) from ABETS where BETNAME ='$*' group by STARTTS order by STARTTS" \
    --output=tmp.dat $(DBNAME)
	cat tmp.dat | awk -F '|' '{i=i+$$2; print $$1,FS,i}' |  head -n-1 >  $*.dat
	@echo "done gnuplot $* - $@ - $<"        



#%.dat : $(BOT_TARGET)/bin/equity 
#	$(BOT_TARGET)/bin/equity --host=$(DBHOST) \
#                                 --database=$(DBNAME) \
#                                 --port=$(DBPORT) \
#                                 --pwd=$(DBPWD) \
#                                 --user=$(DBUSER) \
#                                 --powerdays=$(shell echo $* | cut -d'-' -f2) \
#                                 --betname=$(shell echo $(shell echo $* | cut -d'-' -f1) | tr '[:lower:]' '[:upper:]') \
#                                 --saldo=$(SALDO) > $*.dat         
    

.PHONY : clean
clean :
	rm -rf  *.png 

.PHONY : clean_all
clean_all : clean
	rm -rf *.dat *.green
