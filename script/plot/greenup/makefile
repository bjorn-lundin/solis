###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

UNAME := $(shell uname)


DBHOST := sebjlun-deb64
#DBHOST := 192.168.0.13
#DBHOST := nonodev.com
ifeq ($(DBHOST),localhost)
  DBNAME := bnl
  DBPORT := 5432
  DBPWD  := bnl
  DBUSER := bnl
endif

ifeq ($(DBHOST),192.168.0.13)
  DBNAME := betting
  DBPORT := 5432
  DBPWD  := bnl
  DBUSER := bnl
endif

ifeq ($(DBHOST),nonodev.com)
# Go via ssh tunnel
  HOST := 127.0.0.1
  DBNAME := bnls
  DBPORT := 5433
  #Escape a '$' with a '$'  BettingFotboll1$ is correct pwd
  DBPWD  := BettingFotboll1$$
  DBUSER := bnl
endif

ifeq ($(DBHOST),sebjlun-deb64)
  DBNAME := bnl
  DBPORT := 5432
  DBPWD  := bnl
  DBUSER := bnl
endif


START_DATE := 2012-01-01
STOP_DATE  := 2013-12-31
#SIZE := 30
SALDO := 0
POWERDAYSLIST := 107 207 307 114 214 314 121 221 321 128 228 328 135 235 335


#min/max/delta/hours_before
#  green_10_20_2_2_GB.green \


MIN_MAX_LIST := \
  10_20_1_1 \
  10_20_1_2 \
  10_20_1_3 \
  10_20_1_4 \
  10_20_2_1 \
  10_20_2_2 \
  10_20_2_3 \
  10_20_2_4 \
  10_20_3_1 \
  10_20_3_2 \
  10_20_3_3 \
  10_20_3_4 \
  10_20_4_1 \
  10_20_4_2 \
  10_20_4_3 \
  10_20_4_4 \
  10_20_5_1 \
  10_20_5_2 \
  10_20_5_3 \
  10_20_5_4 \
#  15_25_1_1 \
#  15_25_1_2 \
#  15_25_1_3 \
#  15_25_1_4 \
#  15_25_2_1 \
#  15_25_2_2 \
#  15_25_2_3 \
#  15_25_2_4 \
#  15_25_3_1 \
#  15_25_3_2 \
#  15_25_3_3 \
#  15_25_3_4 \
#  15_25_4_1 \
#  15_25_4_2 \
#  15_25_4_3 \
#  15_25_4_4 \
#  15_25_5_1 \
#  15_25_5_2 \
#  15_25_5_3 \
#  15_25_5_4 \
     
PREFIX1 :=horse
PREFIX2 :=hound
COUNTRY :=GB

BET_NAMES_1 := $(foreach i,$(MIN_MAX_LIST), $(PREFIX1)_$(i)_$(COUNTRY))
BET_NAMES_2 := $(foreach i,$(MIN_MAX_LIST), $(PREFIX2)_$(i)_$(COUNTRY))
BET_NAMES := $(BET_NAMES_1) $(BET_NAMES_2)

PNG_ALLS := $(foreach i,$(BET_NAMES), $(i)_all.png)
PNG_WINS := $(foreach i,$(BET_NAMES), $(i)_win.png)
PNG_PLCS := $(foreach i,$(BET_NAMES), $(i)_plc.png)
PNGS := $(PNG_ALLS) $(PNG_WINS) $(PNG_PLCS) 

#PNGS := $(foreach i,$(BET_NAMES), $(i).png)

PLT_ALLS := $(foreach i,$(BET_NAMES), $(i)_all.plt)
PLT_WINS := $(foreach i,$(BET_NAMES), $(i)_win.plt)
PLT_PLCS := $(foreach i,$(BET_NAMES), $(i)_plc.plt)
PLTS := $(PLT_ALLS) $(PLT_WINS) $(PLT_PLCS) 

#PLTS := $(foreach i,$(BET_NAMES), $(i).plt)

DATS := $(foreach i,$(BET_NAMES), $(i).dat)

#dela upp på plc/win/all

all  : 
	@echo $(PNGS)
	@echo "--------"
plts : $(PLTS)

pngs : $(PNGS)

dats : $(DATS)
    



    
## equity start ##
#save the intermediate files too
.PRECIOUS : %.dat %.plt %.png

.PHONY : split_dats
split_dats :
	/bin/bash create_all_win_plc.bash

# implict rules
# create a dat and then a png file from that dat
# $* is the stem
# $@ is the target file name

#animal_min_max_delta_hours_before
#hound_10_20_4_3_GB.png
%.dat : $(BOT_TARGET)/bin/greening_up_2 
	$(BOT_TARGET)/bin/greening_up_2  \
      --animal=$(shell echo $* | cut -d'_' -f1) \
      --min_odds=$(shell echo $* | cut -d'_' -f2) \
      --max_odds=$(shell echo $* | cut -d'_' -f3) \
      --delta_odds=$(shell echo $* | cut -d'_' -f4) \
      --hours_before_start=$(shell echo $* | cut -d'_' -f5) \
      --start_date=$(START_DATE) \
      --stop_date=$(STOP_DATE) > $@


%.plt : %.dat $(BOT_TARGET)/bin/greening_up_3
	$(BOT_TARGET)/bin/greening_up_3 --filename=$*.dat > $@     
                                    
                                    
%.png : %.plt equity_greening_up.gpl
	gnuplot -e "db_name='bnl'" \
           -e "bet_type='$*'" \
           -e "target_png='$@'" \
           -e "data_file='$*.plt'" equity_greening_up.gpl


#min/max/delta/hours_before
#  green_10_20_2_2_GB.green \

    
    

.PHONY : clean
clean :
	rm -rf  *.png *.plt

.PHONY : clean_all
clean_all : clean
	rm -rf *.dat *.green
