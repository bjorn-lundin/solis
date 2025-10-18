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
    
BET_NAMES_REAL := \
  1.113364952 

BET_NAMES := $(BET_NAMES_REAL)
BET_NAMES := 1.113364952_3171885 1.113364952_4666814 1.113364952_4764558 1.113364952_5218558 1.113364952_5422987 1.113364952_6020219 1.113364952_6295423 1.113364952_7038893 1.113364952_7792126 1.113364952_8186700 1.113364952_8367106 1.113364952_8410029
DATS := 1.113364952*.dat

# do a wordcount of how many items in array
NUMBETS := $(shell echo $(DATS) | wc -w )

all  : 
	./lay_during_race.bash


dat : $(DATS)

test:
	@echo "----bet names---------------"
	@echo $(BET_NAMES)
#	@echo "-----pngs----------------"
#	@echo $(PNGS)
	@echo "-----dats----------------"
	@echo $(DATS)
	@echo "----numbets-----------------"
	@echo $(NUMBETS)
	@echo "---------------------"

PHONY : png
png: $(DATS)
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "numbets='$(NUMBETS)'" \
            -e "target_png='prices_during_race_$(NUMBETS).png'" \
            -e "files='$(BET_NAMES)'" prices_during_race.gpl
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
#    --command="select STARTTS,sum(PROFIT) from ABETS where BETNAME ='$*' group by STARTTS order by STARTTS" \
#    --output=tmp.dat $(DBNAME)
#	cat tmp.dat | awk -F '|' '{i=i+$$2; print $$1,FS,i}' |  head -n-1 >  $*.dat
#	@echo "done gnuplot $* - $@ - $<"        



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
	rm -rf  *.dat 

.PHONY : clean_all
clean_all : clean
	rm -rf *.dat *.green
