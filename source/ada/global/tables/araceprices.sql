begin;
create table ARACEPRICES ( 
  PRICETS timestamp(3) without time zone  not null , -- Primary Key
  MARKETID varchar(11) default ' ' not null , -- Primary Key
  SELECTIONID integer default 1 not null , -- Primary Key
  STATUS varchar(50) default ' ' not null , -- non unique index 4
  BACKPRICE numeric(15,2) default 0.0 not null , -- non unique index 5
  LAYPRICE numeric(15,2) default 0.0 not null , -- non unique index 6
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null ) -- non unique index 7 
;

alter table ARACEPRICES add constraint ARACEPRICESP1 primary key (
  pricets,marketid,selectionid
)
;

create index ARACEPRICESI2 on ARACEPRICES (
  selectionid
)
;

create index ARACEPRICESI3 on ARACEPRICES (
  backprice
)
;

create index ARACEPRICESI4 on ARACEPRICES (
  layprice
)
;

create index ARACEPRICESI5 on ARACEPRICES (
  status
)
;

create index ARACEPRICESI6 on ARACEPRICES (
  marketid
)
;

create index ARACEPRICESI7 on ARACEPRICES (
  pricets
)
;

create index ARACEPRICESI8 on ARACEPRICES (
  ixxluts
)
;

comment on table ARACEPRICES is 'runners odds at race';
comment on column ARACEPRICES.pricets is 'timestamp of prices';
comment on column ARACEPRICES.marketid is 'market id';
comment on column ARACEPRICES.selectionid is 'selection_id';
comment on column ARACEPRICES.status is 'status';
comment on column ARACEPRICES.backprice is 'back_price';
comment on column ARACEPRICES.layprice is 'lay_price';
comment on column ARACEPRICES.ixxlupd is 'Latest updater';
comment on column ARACEPRICES.ixxluts is 'Latest update timestamp';
commit;

