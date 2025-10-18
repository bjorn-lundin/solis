###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

include ../makefile.inc

PNGS  := $(foreach NAME,$(ALL_CORRECT_SCORES),$(NAME).png)
DATS  := $(foreach NAME,$(ALL_CORRECT_SCORES),$(NAME).dat)

.PHONY :dats all test clean
all  :  $(PNGS)

png : $(PNGS)
dat : $(DATS) 

test:
	@echo "|"
	@echo $(PNGS)
	@echo "|"
	@echo $(DATS)
	@echo "|"
	@echo $(CC_LIST)
	@echo "|"
    
## equity start ##
#save the intermediate files too
.PRECIOUS : %.dat

#dats :
#	echo "Would rerun all dats"
#	$(BOT_SCRIPT)/bash/run_lay_football.bash
    
%.dat : $(BOT_TARGET)/bin/lay_football
	$(BOT_TARGET)/bin/lay_football --hostname=$(DBHOST) --database=$(DBNAME) --username=$(DBUSER) --password=$(DBPWD) \
                                 --market_type=CORRECT_SCORE \
                                 --country_code=$(shell echo $* | cut -d'_' -f3) \
                                 --min_odds_score=4  \
                                 --max_odds_score=35 \
                                 --min_odds_match=2  \
                                 --max_odds_match=7.9 > $*.dat         

                                 # implict rules
# create a dat and then a png file from that dat
# $* is the stem
# $@ is the target file name

%.png : %.dat ../lay_football.gpl
	gnuplot -e "db_name='$(DBNAME)'" \
            -e "bet_type='$*'" \
            -e "target_png='$@'" \
            -e "data_file='$*.dat'" ../lay_football.gpl
#	@echo "done gnuplot $* - $@ - $<"        

clean :
	rm -rf  *.png *.dat
