begin;
create table ARUNNERS ( 
  MARKETID varchar(11) default ' ' not null , -- Primary Key
  SELECTIONID integer default 1 not null , -- Primary Key
  SORTPRIO integer default 1 not null , 
  STATUS varchar(50) default ' ' not null , -- non unique index 3
  HANDICAP numeric(15,2) default 0.0 not null , 
  RUNNERNAME varchar(50) default ' ' not null , 
  RUNNERNAMESTRIPPED varchar(50) default ' ' , -- non unique index 4
  RUNNERNAMENUM varchar(2) default ' ' , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null ) -- non unique index 5 
;

alter table ARUNNERS add constraint ARUNNERSP1 primary key (
  marketid,selectionid
)
;

create index ARUNNERSI2 on ARUNNERS (
  runnernamestripped
)
;

create index ARUNNERSI3 on ARUNNERS (
  marketid,status
)
;

create index ARUNNERSI4 on ARUNNERS (
  status
)
;

create index ARUNNERSI5 on ARUNNERS (
  ixxluts
)
;

comment on table ARUNNERS is 'collected runners';
comment on column ARUNNERS.marketid is 'market id';
comment on column ARUNNERS.selectionid is 'selection_id';
comment on column ARUNNERS.sortprio is 'sortprio';
comment on column ARUNNERS.status is 'status';
comment on column ARUNNERS.handicap is 'handicap';
comment on column ARUNNERS.runnername is 'runner_name';
comment on column ARUNNERS.runnernamestripped is 'runner_name without startnum';
comment on column ARUNNERS.runnernamenum is 'startnum in runner name';
comment on column ARUNNERS.ixxlupd is 'Latest updater';
comment on column ARUNNERS.ixxluts is 'Latest update timestamp';
commit;

