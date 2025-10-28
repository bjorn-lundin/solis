begin;
create table smhi ( 
  TIME_START timestamp(3) without time zone  not null , -- Primary Key
  TIME_STOP timestamp(3) without time zone  not null , 
  AIR_TEMPERATURE numeric(15,2) default 0.0 not null , 
  WIND_SPEED numeric(15,2) default 0.0 not null , 
  CLOUD_COVERAGE integer default 1 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table smhi add constraint smhiP1 primary key (
  time_start
)
;

comment on table smhi is 'logged wheater data';
comment on column smhi.time_start is 'start time';
comment on column smhi.time_stop is 'timestamp';
comment on column smhi.air_temperature is 'temp value';
comment on column smhi.wind_speed is 'wind speed value';
comment on column smhi.cloud_coverage is 'cloud area fraction value';
comment on column smhi.ixxlupd is 'Latest updater';
comment on column smhi.ixxluts is 'Latest update timestamp';
commit;

