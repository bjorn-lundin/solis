begin;
create table solis ( 
  TIME_START timestamp(3) without time zone  not null , -- Primary Key
  TIME_STOP timestamp(3) without time zone  not null , 
  POWER1 numeric(15,2) default 0.0 not null , 
  POWER2 numeric(15,2) default 0.0 not null , 
  BATTERY_POWER numeric(15,2) default 0.0 not null , 
  BATTERY_CHARGE numeric(15,2) default 0.0 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table solis add constraint solisP1 primary key (
  time_start
)
;

comment on table solis is 'logged inverter data';
comment on column solis.time_start is 'start time';
comment on column solis.time_stop is 'timestamp';
comment on column solis.power1 is 'power line 1 W';
comment on column solis.power2 is 'power line 2 W';
comment on column solis.battery_power is 'battery power kWh';
comment on column solis.battery_charge is 'battery charge %';
comment on column solis.ixxlupd is 'Latest updater';
comment on column solis.ixxluts is 'Latest update timestamp';
commit;

