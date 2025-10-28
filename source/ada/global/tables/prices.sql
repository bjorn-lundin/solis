begin;
create table prices ( 
  TIME_START timestamp(3) without time zone  not null , -- Primary Key
  TIME_STOP timestamp(3) without time zone  not null , 
  SEK_PER_KWH numeric(15,2) default 0.0 not null , 
  EUR_PER_KWH numeric(15,2) default 0.0 not null , 
  EXCHANGE_RATE numeric(15,2) default 0.0 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table prices add constraint pricesP1 primary key (
  time_start
)
;

comment on table prices is 'logged electricity prices';
comment on column prices.time_start is 'start time';
comment on column prices.time_stop is 'timestamp';
comment on column prices.sek_per_kwh is 'sek/kWh';
comment on column prices.eur_per_kwh is 'eur/kWh';
comment on column prices.exchange_rate is 'exchange rate';
comment on column prices.ixxlupd is 'Latest updater';
comment on column prices.ixxluts is 'Latest update timestamp';
commit;

