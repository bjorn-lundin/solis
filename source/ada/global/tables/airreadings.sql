begin;
create table airreadings ( 
  MACADDRESS varchar(6) default ' ' not null , -- Primary Key
  CREATED timestamp(3) without time zone  not null , -- Primary Key
  TEMPERATURE numeric(15,2) default 0.0 not null , 
  PRESSURE integer default 1 not null , 
  HUMIDITY numeric(15,2) default 0.0 not null , 
  GASRESISTANCE integer default 1 not null , 
  ALTITUDE numeric(15,2) default 0.0 , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table airreadings add constraint airreadingsP1 primary key (
  macaddress,created
)
;

comment on table airreadings is 'logged readings airquality';
comment on column airreadings.macaddress is 'mac id';
comment on column airreadings.created is 'timestamp';
comment on column airreadings.temperature is 'raw temp value';
comment on column airreadings.pressure is 'raw pressure value';
comment on column airreadings.humidity is 'raw humidity value';
comment on column airreadings.gasresistance is 'raw gasresistance value';
comment on column airreadings.altitude is 'raw altitude value';
comment on column airreadings.ixxlupd is 'Latest updater';
comment on column airreadings.ixxluts is 'Latest update timestamp';
commit;

