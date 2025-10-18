begin;
create table OKMARKETS ( 
  MARKETID varchar(11) default ' ' not null , -- Primary Key
  EVENTID varchar(11) default ' ' not null , -- non unique index 2
  MARKETTYPE varchar(25) default ' ' not null , -- non unique index 3
  NUMWINNERS integer default 1 not null , -- non unique index 4
  NUMRUNNERS integer default 1 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table OKMARKETS add constraint OKMARKETSP1 primary key (
  marketid
)
;

create index OKMARKETSI2 on OKMARKETS (
  eventid
)
;

create index OKMARKETSI3 on OKMARKETS (
  markettype
)
;

create index OKMARKETSI4 on OKMARKETS (
  numwinners
)
;

comment on table OKMARKETS is 'ok collected runners';
comment on column OKMARKETS.marketid is 'market id';
comment on column OKMARKETS.eventid is 'event id';
comment on column OKMARKETS.markettype is 'WINNER/PLACE';
comment on column OKMARKETS.numwinners is 'num winners';
comment on column OKMARKETS.numrunners is 'num runners';
comment on column OKMARKETS.ixxlupd is 'Latest updater';
comment on column OKMARKETS.ixxluts is 'Latest update timestamp';
commit;

