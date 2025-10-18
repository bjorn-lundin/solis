###############################################################################
#                                                                             #
#                                Make File                                    #
#                                                                             #
###############################################################################

UNAME := $(shell uname)

#DBHOST := sebjlun-deb64
#DBHOST := 192.168.0.13
DBHOST := nonodev.com

ifeq ($(DBHOST),nonodev.com)
  DBNAME :=bnls
#  DBPORT := 5432
  DBPWD  :=BettingFotboll1$$
  DBUSER :=bnl
endif

ifeq ($(DBHOST),localhost)
  DBNAME := bnl
#  DBPORT := 5432
  DBPWD  := bnl
  DBUSER := bnl
endif

ifeq ($(DBHOST),sebjlun-deb64)
  DBNAME := bnl
#  DBPORT := 5432
  DBPWD  := bnl
  DBUSER := bnl
endif

#CC_LIST:=XX BR GB ES IT FR MX NL IL PT DE TR BE IS CO CL GR QA SA RU AE EE IN EG EC PA MY AR \
#BH LV ZA CR FI DZ UY CH JM BO MT AG AZ VE AW PS HK IR VN US ID TT SG HN OM HR PE MA KW NO PY TJ YY

CC_LIST:=XX BR GB ES IT FR MX NL IL PT DE TR YY

ALL_HALF_TIME_SCORES := $(foreach COUNTRY_CODE,$(CC_LIST),half_time_score_$(COUNTRY_CODE))
ALL_CORRECT_SCORES   := $(foreach COUNTRY_CODE,$(CC_LIST),correct_score_$(COUNTRY_CODE))

PNGS  := $(foreach NAME,$(ALL_SCORES),$(NAME).png)
DATS  := $(foreach NAME,$(ALL_SCORES),$(NAME).dat)
