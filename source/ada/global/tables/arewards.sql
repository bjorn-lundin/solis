begin;
create table AREWARDS ( 
  MARKETID varchar(11) default ' ' not null , -- Primary Key
  SELECTIONID integer default 1 not null , -- Primary Key
  PRICETS timestamp(3) without time zone  not null , -- Primary Key
  SIDE varchar(4) default ' ' not null , -- Primary Key
  PROFIT numeric(15,2) default 0.0 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table AREWARDS add constraint AREWARDSP1 primary key (
  marketid,selectionid,pricets,side
)
;

comment on table AREWARDS is 'rewards betting 100 at this time';
comment on column AREWARDS.marketid is 'market id';
comment on column AREWARDS.selectionid is 'selection_id';
comment on column AREWARDS.pricets is 'timestamp of prices';
comment on column AREWARDS.side is 'side';
comment on column AREWARDS.profit is 'the reward';
comment on column AREWARDS.ixxlupd is 'Latest updater';
comment on column AREWARDS.ixxluts is 'Latest update timestamp';
commit;

