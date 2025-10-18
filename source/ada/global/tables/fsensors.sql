begin;
create table FSENSORS ( 
  MACADDRESS varchar(6) default ' ' not null , -- Primary Key
  POTNAME varchar(30) default ' ' not null , 
  THRESHOLD integer default 1 not null , 
  LASTNOTIFY timestamp(3) without time zone  not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table FSENSORS add constraint FSENSORSP1 primary key (
  macaddress
)
;

comment on table FSENSORS is 'sensor settings';
comment on column FSENSORS.macaddress is 'mac id';
comment on column FSENSORS.potname is 'name of pot';
comment on column FSENSORS.threshold is 'threshold to mail';
comment on column FSENSORS.lastnotify is 'last time mailed';
comment on column FSENSORS.ixxlupd is 'Latest updater';
comment on column FSENSORS.ixxluts is 'Latest update timestamp';
commit;

