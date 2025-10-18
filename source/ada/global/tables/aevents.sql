begin;
create table AEVENTS ( 
  EVENTID varchar(11) default ' ' not null , -- Primary Key
  EVENTNAME varchar(50) default ' ' not null , 
  COUNTRYCODE varchar(2) default ' ' not null , -- non unique index 2
  TIMEZONE varchar(50) default ' ' not null , 
  OPENTS timestamp(3) without time zone  not null , -- non unique index 3
  EVENTTYPEID integer default 1 not null , -- non unique index 4
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null ) -- non unique index 5 
;

alter table AEVENTS add constraint AEVENTSP1 primary key (
  eventid
)
;

create index AEVENTSI2 on AEVENTS (
  countrycode
)
;

create index AEVENTSI3 on AEVENTS (
  opents
)
;

create index AEVENTSI4 on AEVENTS (
  eventtypeid
)
;

create index AEVENTSI5 on AEVENTS (
  ixxluts
)
;

comment on table AEVENTS is 'collected runners';
comment on column AEVENTS.eventid is 'event id';
comment on column AEVENTS.eventname is 'event name';
comment on column AEVENTS.countrycode is 'code of contry';
comment on column AEVENTS.timezone is 'timezone';
comment on column AEVENTS.opents is 'timestamp open';
comment on column AEVENTS.eventtypeid is 'event type id (7,4339)';
comment on column AEVENTS.ixxlupd is 'Latest updater';
comment on column AEVENTS.ixxluts is 'Latest update timestamp';
commit;

