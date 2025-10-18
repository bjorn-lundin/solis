begin;
create table ASTARTTIMES ( 
  STARTTIME timestamp(3) without time zone  not null , -- Primary Key
  VENUE varchar(30) default ' ' not null , -- Primary Key
  MARKETNAME varchar(50) default ' ' not null )  
;

alter table ASTARTTIMES add constraint ASTARTTIMESP1 primary key (
  starttime,venue
)
;

comment on table ASTARTTIMES is 'Starttimes';
comment on column ASTARTTIMES.starttime is 'start of race';
comment on column ASTARTTIMES.venue is 'Venue name';
comment on column ASTARTTIMES.marketname is 'market name';
commit;

