begin;
create table ABETS ( 
  BETID bigint default 1 not null , -- Primary Key
  MARKETID varchar(11) default ' ' not null , -- non unique index 2
  BETMODE integer default 1 not null , 
  POWERDAYS integer default 1 not null , -- non unique index 3
  SELECTIONID integer default 1 not null , 
  REFERENCE varchar(30) default ' ' not null , -- non unique index 4
  SIZE numeric(15,2) default 0.0 not null , 
  PRICE numeric(15,2) default 0.0 not null , 
  SIDE varchar(4) default ' ' not null , 
  BETNAME varchar(100) default ' ' not null , -- non unique index 5
  BETWON boolean default False , -- non unique index 6
  PROFIT numeric(15,2) default 0.0 not null , 
  STATUS varchar(50) default ' ' not null , -- non unique index 7
  EXESTATUS varchar(50) default ' ' not null , 
  EXEERRCODE varchar(50) default ' ' not null , 
  INSTSTATUS varchar(50) default ' ' not null , 
  INSTERRCODE varchar(50) default ' ' not null , 
  STARTTS timestamp(3) without time zone  , -- non unique index 8
  BETPLACED timestamp(3) without time zone  , -- non unique index 9
  PRICEMATCHED numeric(15,2) default 0.0 not null , 
  SIZEMATCHED numeric(15,2) default 0.0 not null , 
  RUNNERNAME varchar(50) default ' ' not null , 
  FULLMARKETNAME varchar(50) default ' ' not null , 
  SVNREVISION varchar(40) default ' ' not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table ABETS add constraint ABETSP1 primary key (
  betid
)
;

create index ABETSI2 on ABETS (
  marketid
)
;

create index ABETSI3 on ABETS (
  betwon
)
;

create index ABETSI4 on ABETS (
  powerdays
)
;

create index ABETSI5 on ABETS (
  betplaced
)
;

create index ABETSI6 on ABETS (
  startts
)
;

create index ABETSI7 on ABETS (
  betname
)
;

create index ABETSI8 on ABETS (
  status
)
;

create index ABETSI9 on ABETS (
  reference
)
;

create index ABETSI10 on ABETS (
  date_part('year'::text, betplaced)
)
;

create index ABETSI11 on ABETS (
  date_part('month'::text, betplaced)
)
;

create index ABETSI12 on ABETS (
  date_part('day'::text, betplaced)
)
;

comment on table ABETS is 'Bets';
comment on column ABETS.betid is 'bet id';
comment on column ABETS.marketid is 'market id';
comment on column ABETS.betmode is 'dry,real,sim';
comment on column ABETS.powerdays is 'power of denominator';
comment on column ABETS.selectionid is 'selection id';
comment on column ABETS.reference is 'reference';
comment on column ABETS.size is 'wanted size';
comment on column ABETS.price is 'wanted price';
comment on column ABETS.side is 'BACK/LAY';
comment on column ABETS.betname is 'name of bet';
comment on column ABETS.betwon is 'success';
comment on column ABETS.profit is 'vinst';
comment on column ABETS.status is 'status';
comment on column ABETS.exestatus is 'execution status';
comment on column ABETS.exeerrcode is 'execution errcode';
comment on column ABETS.inststatus is 'intruction status';
comment on column ABETS.insterrcode is 'intruction errcode';
comment on column ABETS.startts is 'start ts according to market';
comment on column ABETS.betplaced is 'when bet was placed';
comment on column ABETS.pricematched is 'matched price';
comment on column ABETS.sizematched is 'size price';
comment on column ABETS.runnername is 'name of runner';
comment on column ABETS.fullmarketname is 'name of market';
comment on column ABETS.svnrevision is 'svn revision of bot';
comment on column ABETS.ixxlupd is 'Latest updater';
comment on column ABETS.ixxluts is 'Latest update timestamp';
commit;

