/*
Created		26.7.2005
Modified		26.7.2005
Project		
Model			
Company		
Author		
Version		
Database		MS SQL 2000 
*/

Drop table [soubory] 
go
Drop table [adresare] 
go
Drop table [disky] 
go


Create table [disky]
(
	[id] Integer NOT NULL,
	[jmeno] Varchar(256) NULL,
Primary Key ([id])
) 
go

Create table [adresare]
(
	[id] Integer NOT NULL,
	[adresar] Varchar(256) NULL,
	[iddisku] Integer NOT NULL,
Primary Key ([id])
) 
go

Create table [soubory]
(
	[id] Integer NOT NULL,
	[soubor] Varchar(256) NULL,
	[idadr] Integer NOT NULL,
Primary Key ([id])
) 
go


Set quoted_identifier on
go






Set quoted_identifier off
go


