create database lab CHARACTER SET utf8 COLLATE utf8_bin;

create table Items (id int not null auto_increment, name varchar(300), price decimal (10, 2), count decimal (10, 2), primary key (id));
insert into Items (name, count, price) values ('Milk', 100, 290);
