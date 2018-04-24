# DiskBase pro Linux

## What it is

Linux port of free Windows DiskBase (https://sourceforge.net/projects/diskbase/), disk cataloging program, i.e. it is able to read all the file and folder names from a disk and save it to its database.

## Co to je

Linux verze free windows DiskBase (https://sourceforge.net/projects/diskbase/) , program pro vedení katalogu disků.

## Status

Stable functions: 
- rerieve and display the database disks
- file search
- data export
- retrieving new data into the database

Untested functions:
- print data

## Stav

Stabilní funkce: 
- načtení a zobrazení databáze disků
- vyhledávání souborů
- exporty dat
- načtení nových dat do databáze

Netestované (nestabilní) funkce: 
- tisk dat

## Requirements
- Lazarus 1.6.2+, developed on version 1.8
- compiler FPC 3.0.0+, developed on version 3.0.4
- Linux desktop
- util lsblk, package util-linux
- run command visudo and add row "user  ALL= NOPASSWD: /usr/bin/lsblk", where "user"  the currently logged on user in Linux (Joe's, joe, admin ..) 


## Požadavky
- Lazarus (FPC) kompilátor
- Linux prostředí
- utilitka lsblk, balíček util-linux
- pustit příkaz visudo a zapsat řádek "user  ALL= NOPASSWD: /usr/bin/lsblk", kde "user" je aktuálně přihlášený uživatel v linuxu (pepik,franta,admin,..)


## How to compile
- install Lazarus (1.6 or above) from binaries - http://wiki.freepascal.org/Installing_Lazarus
- get source code from https://github.com/ibv/DiskBase
- install atgauge_package.lpk from SOURCE/atgauge_package
- compile dynamic library from SOURCE/DLLs, run  script "build" in SOURCE/DLLs
- in Lazarus IDE, menu - File -> Open -> "/path/to/DiskBase.lpi"
- in Lazarus IDE, menu - Run -> F9 (run) or Ctrl+F9 (compile) or Shift+F9 (link) 
- or run "/usr/bin/lazbuild /path/to/DiskBase.lpi"


## Jak zkompilovat
- nainstalovat Lazarus (1.6 a vyšší, 1.8RC5 dop.) dle návodu - http://wiki.freepascal.org/Installing_Lazarus
- získat zdrojový kod z https://github.com/ibv/DiskBase
- instalace atgauge_package.lpk z adreáře SOURCE/atgauge_package
- zkompilovat podpůrné dynamické knihovny ze SOURCE/DLLs, pustit skript "build" v SOURCE/DLLs
- v Lazarus IDE, menu - File -> Open -> "/cesta/k/DiskBase.lpi"
  v Lazarus IDE, menu - Run -> F9 (run) or Ctrl+F9 (compile) or Shift+F9 (link)
- nebo pomocí "/usr/bin/lazbuild /cesta/k/DiskBase.lpi"

## Further information

For more information about it, please visit:
> http://ivb.sweb.cz/en/diskbase.html
