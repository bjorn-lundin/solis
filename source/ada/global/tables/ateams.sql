begin;
create table ATEAMS ( 
  TEAMID integer default 1 not null , -- Primary Key
  TEAMNAME varchar(50) default ' ' not null , 
  COUNTRYCODE varchar(2) default ' ' not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table ATEAMS add constraint ATEAMSP1 primary key (
  teamid
)
;

comment on table ATEAMS is 'Teams';
comment on column ATEAMS.teamid is 'Team id';
comment on column ATEAMS.teamname is 'Team name';
comment on column ATEAMS.countrycode is 'code of contry';
comment on column ATEAMS.ixxlupd is 'Latest updater';
comment on column ATEAMS.ixxluts is 'Latest update timestamp';
commit;

