create database lab CHARACTER SET utf8 COLLATE utf8_bin;

create table Items (id int not null auto_increment, name varchar(300), price decimal (10, 2), count decimal (10, 2), primary key (id));
insert into Items (name, count, price) values ('Сыр', 100, 290);
 
create table Users (id int not null auto_increment, name varchar(300), password varchar(32), primary key (id));
insert into Users (name, password) values ('semigroupoid', 'fa9b8c8a99d12f10c96e3c96c2ba5ae3');
insert into Users (name, password) values ('admin', '21232f297a57a5a743894a0e4a801fc3');
