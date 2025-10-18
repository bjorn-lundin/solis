begin;
create table APRICES ( 
  MARKETID varchar(11) default ' ' not null , -- Primary Key
  SELECTIONID integer default 1 not null , -- Primary Key
  PRICETS timestamp(3) without time zone  not null , 
  STATUS varchar(50) default ' ' not null , -- non unique index 3
  TOTALMATCHED numeric(15,2) default 0.0 not null , 
  BACKPRICE numeric(15,2) default 0.0 not null , -- non unique index 4
  LAYPRICE numeric(15,2) default 0.0 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null ) -- non unique index 5 
;

alter table APRICES add constraint APRICESP1 primary key (
  marketid,selectionid
)
;

create index APRICESI2 on APRICES (
  backprice
)
;

create index APRICESI3 on APRICES (
  status
)
;

create index APRICESI4 on APRICES (
  ixxluts
)
;

comment on table APRICES is 'runners odds';
comment on column APRICES.marketid is 'market id';
comment on column APRICES.selectionid is 'selection_id';
comment on column APRICES.pricets is 'timestamp of prices';
comment on column APRICES.status is 'status';
comment on column APRICES.totalmatched is 'tot matched';
comment on column APRICES.backprice is 'back_price';
comment on column APRICES.layprice is 'lay_price';
comment on column APRICES.ixxlupd is 'Latest updater';
comment on column APRICES.ixxluts is 'Latest update timestamp';
commit;

