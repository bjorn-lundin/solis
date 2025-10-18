begin;
create table APRICESHISTORY ( 
  MARKETID varchar(11) default ' ' not null , -- Primary Key
  SELECTIONID integer default 1 not null , -- Primary Key
  PRICETS timestamp(3) without time zone  not null , -- Primary Key
  STATUS varchar(50) default ' ' not null , -- non unique index 4
  TOTALMATCHED numeric(15,2) default 0.0 not null , 
  BACKPRICE numeric(15,2) default 0.0 not null , -- non unique index 5
  LAYPRICE numeric(15,2) default 0.0 not null , -- non unique index 6
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table APRICESHISTORY add constraint APRICESHISTORYP1 primary key (
  marketid,selectionid,pricets
)
;

create index APRICESHISTORYI2 on APRICESHISTORY (
  marketid,pricets
)
;

create index APRICESHISTORYI3 on APRICESHISTORY (
  backprice
)
;

create index APRICESHISTORYI4 on APRICESHISTORY (
  layprice
)
;

create index APRICESHISTORYI5 on APRICESHISTORY (
  status
)
;

create index APRICESHISTORYI6 on APRICESHISTORY (
  pricets
)
;

create index APRICESHISTORYI7 on APRICESHISTORY (
  marketid
)
;

comment on table APRICESHISTORY is 'runners odds during race';
comment on column APRICESHISTORY.marketid is 'market id';
comment on column APRICESHISTORY.selectionid is 'selection_id';
comment on column APRICESHISTORY.pricets is 'timestamp of prices';
comment on column APRICESHISTORY.status is 'status';
comment on column APRICESHISTORY.totalmatched is 'tot matched';
comment on column APRICESHISTORY.backprice is 'back price';
comment on column APRICESHISTORY.layprice is 'lay price';
comment on column APRICESHISTORY.ixxlupd is 'Latest updater';
comment on column APRICESHISTORY.ixxluts is 'Latest update timestamp';
commit;

