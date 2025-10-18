begin;
create table FREADINGS ( 
  MACADDRESS varchar(6) default ' ' not null , -- Primary Key
  CREATED timestamp(3) without time zone  not null , -- Primary Key
  READING integer default 1 not null , 
  IXXLUPD varchar(15) default ' ' not null , 
  IXXLUTS timestamp(3) without time zone  not null )  
;

alter table FREADINGS add constraint FREADINGSP1 primary key (
  macaddress,created
)
;

comment on table FREADINGS is 'logged readings';
comment on column FREADINGS.macaddress is 'mac id';
comment on column FREADINGS.created is 'name of pot';
comment on column FREADINGS.reading is 'raw sensor value';
comment on column FREADINGS.ixxlupd is 'Latest updater';
comment on column FREADINGS.ixxluts is 'Latest update timestamp';
commit;

